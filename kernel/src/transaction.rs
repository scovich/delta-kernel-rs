use std::sync::Arc;

use crate::actions::visitors::TransactionVisitor;
use crate::actions::{get_log_schema, Transaction, TRANSACTION_NAME};
use crate::snapshot::Snapshot;
use crate::{DeltaResult, Engine, EngineData, Expression as Expr, SchemaRef};

pub use crate::actions::visitors::TransactionMap;
pub struct TransactionScanner {
    snapshot: Arc<Snapshot>,
}

impl TransactionScanner {
    pub fn new(snapshot: Arc<Snapshot>) -> Self {
        TransactionScanner { snapshot }
    }

    /// Scan the entire log for all application ids but terminate early if a specific application id is provided
    fn scan_application_transactions(
        &self,
        engine: &dyn Engine,
        application_id: Option<&str>,
    ) -> DeltaResult<TransactionMap> {
        let schema = Self::get_txn_schema()?;
        let mut visitor = TransactionVisitor::new(application_id.map(|s| s.to_owned()));
        for maybe_data in self.replay_for_app_ids(engine, schema.clone(), application_id)? {
            let (txns, _) = maybe_data?;
            txns.extract(schema.clone(), &mut visitor)?;
            // if a specific id is requested and a transaction was found, then return
            if application_id.is_some() && !visitor.transactions.is_empty() {
                break;
            }
        }

        Ok(visitor.transactions)
    }

    // Factored out to facilitate testing
    fn get_txn_schema() -> DeltaResult<SchemaRef> {
        get_log_schema().project(&[TRANSACTION_NAME])
    }

    // Factored out to facilitate testing
    fn replay_for_app_ids(
        &self,
        engine: &dyn Engine,
        schema: SchemaRef,
        application_id: Option<&str>,
    ) -> DeltaResult<impl Iterator<Item = DeltaResult<(Box<dyn EngineData>, bool)>> + Send> {
        // when all ids are requested then a full scan of the log to the latest checkpoint is required
        let app_id_col = Expr::column("txn.appId");
        let meta_predicate = match application_id {
            Some(id) => app_id_col.eq(Expr::literal(id)),
            None => app_id_col.is_not_null(),
        };
        self.snapshot
            .log_segment
            .replay(engine, schema.clone(), schema, Some(meta_predicate))
    }

    /// Scan the Delta Log for the latest transaction entry of an application
    pub fn application_transaction(
        &self,
        engine: &dyn Engine,
        application_id: &str,
    ) -> DeltaResult<Option<Transaction>> {
        let mut transactions = self.scan_application_transactions(engine, Some(application_id))?;
        Ok(transactions.remove(application_id))
    }

    /// Scan the Delta Log to obtain the latest transaction for all applications
    pub fn application_transactions(&self, engine: &dyn Engine) -> DeltaResult<TransactionMap> {
        self.scan_application_transactions(engine, None)
    }
}

#[cfg(all(test, feature = "default-engine"))]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::engine::sync::SyncEngine;
    use crate::Table;
    use itertools::Itertools;

    fn get_latest_transactions(path: &str, app_id: &str) -> (TransactionMap, Option<Transaction>) {
        let path = std::fs::canonicalize(PathBuf::from(path)).unwrap();
        let url = url::Url::from_directory_path(path).unwrap();
        let engine = SyncEngine::new();

        let table = Table::new(url);
        let snapshot = table.snapshot(&engine, None).unwrap();
        let txn_scan = TransactionScanner::new(snapshot.into());

        (
            txn_scan.application_transactions(&engine).unwrap(),
            txn_scan.application_transaction(&engine, app_id).unwrap(),
        )
    }

    #[test]
    fn test_txn() {
        let (txns, txn) = get_latest_transactions("./tests/data/basic_partitioned/", "test");
        assert!(txn.is_none());
        assert_eq!(txns.len(), 0);

        let (txns, txn) = get_latest_transactions("./tests/data/app-txn-no-checkpoint/", "my-app");
        assert!(txn.is_some());
        assert_eq!(txns.len(), 2);
        assert_eq!(txns.get("my-app"), txn.as_ref());
        assert_eq!(
            txns.get("my-app2"),
            Some(Transaction {
                app_id: "my-app2".to_owned(),
                version: 2,
                last_updated: None
            })
            .as_ref()
        );

        let (txns, txn) = get_latest_transactions("./tests/data/app-txn-checkpoint/", "my-app");
        assert!(txn.is_some());
        assert_eq!(txns.len(), 2);
        assert_eq!(txns.get("my-app"), txn.as_ref());
        assert_eq!(
            txns.get("my-app2"),
            Some(Transaction {
                app_id: "my-app2".to_owned(),
                version: 2,
                last_updated: None
            })
            .as_ref()
        );
    }

    #[test]
    fn test_replay_for_app_ids() {
        let path = std::fs::canonicalize(PathBuf::from("./tests/data/parquet_row_group_skipping/"));
        let url = url::Url::from_directory_path(path.unwrap()).unwrap();
        let engine = SyncEngine::new();

        let table = Table::new(url);
        let snapshot = table.snapshot(&engine, None).unwrap();
        let txn = TransactionScanner::new(snapshot.into());
        let txn_schema = TransactionScanner::get_txn_schema().unwrap();

        // The checkpoint has five parts, each containing one action. There are two app ids.
        let data: Vec<_> = txn
            .replay_for_app_ids(&engine, txn_schema.clone(), None)
            .unwrap()
            .try_collect()
            .unwrap();
        assert_eq!(data.len(), 2);

        let data: Vec<_> = txn
            .replay_for_app_ids(
                &engine,
                txn_schema.clone(),
                Some("3ae45b72-24e1-865a-a211-34987ae02f2a"),
            )
            .unwrap()
            .try_collect()
            .unwrap();
        assert_eq!(data.len(), 1);

        // This one will not be found (missing character)
        let data: Vec<_> = txn
            .replay_for_app_ids(
                &engine,
                txn_schema,
                Some("3ae45b72-24e1-865a-a211-34987ae02f2"),
            )
            .unwrap()
            .try_collect()
            .unwrap();
        assert_eq!(data.len(), 0);
    }
}
