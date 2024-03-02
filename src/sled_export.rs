//! # sled -> sqlite migration code
//!
//! Before the 0.0.9 release, iamb used matrix-sdk@0.6.2, which used [sled]
//! for storing information, including room keys. In matrix-sdk@0.7.0,
//! the SDK switched to using SQLite. This module takes care of opening
//! sled, exporting the inbound group sessions used for decryption,
//! and importing them into SQLite.
//!
//! This code will eventually be removed once people have been given enough
//! time to upgrade off of pre-0.0.9 versions.
//!
//! [sled]: https://docs.rs/sled/0.34.7/sled/index.html
use sled::{Config, IVec};
use std::path::Path;

use crate::base::IambError;
use matrix_sdk::crypto::olm::{ExportedRoomKey, InboundGroupSession, PickledInboundGroupSession};

#[derive(Debug, thiserror::Error)]
pub enum SledMigrationError {
    #[error("sled failure: {0}")]
    Sled(#[from] sled::Error),

    #[error("deserialization failure: {0}")]
    Deserialize(#[from] serde_json::Error),
}

fn group_session_from_slice(
    (_, bytes): (IVec, IVec),
) -> Result<PickledInboundGroupSession, SledMigrationError> {
    serde_json::from_slice(&bytes).map_err(SledMigrationError::from)
}

async fn export_room_keys_priv(
    sled_dir: &Path,
) -> Result<Vec<ExportedRoomKey>, SledMigrationError> {
    let path = sled_dir.join("matrix-sdk-state");
    let store = Config::new().temporary(false).path(&path).open()?;
    let inbound_groups = store.open_tree("inbound_group_sessions")?;

    let mut exported = vec![];
    let sessions = inbound_groups
        .iter()
        .map(|p| p.map_err(SledMigrationError::from).and_then(group_session_from_slice))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter_map(|p| InboundGroupSession::from_pickle(p).ok());

    for session in sessions {
        exported.push(session.export().await);
    }

    Ok(exported)
}

pub async fn export_room_keys(sled_dir: &Path) -> Result<Vec<ExportedRoomKey>, IambError> {
    export_room_keys_priv(sled_dir).await.map_err(IambError::from)
}
