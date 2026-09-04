use matrix_sdk::Client;
use matrix_sdk::encryption::verification::{
    Verification,
    VerificationRequest,
    VerificationRequestState,
};
use matrix_sdk::ruma::events::key::verification::VerificationMethod;
use matrix_sdk::ruma::{OwnedDeviceId, OwnedUserId, UserId};
use modalkit::errors::UIError;
use modalkit::prelude::{EditInfo, InfoMessage};

use crate::base::{AsyncProgramStore, IambError, IambResult, ProgramStore, VerifyAction};

const SUPPORTED_METHODS: [VerificationMethod; 3] = [
    // Emoji verification
    VerificationMethod::SasV1,
    // QR Code verification
    VerificationMethod::ReciprocateV1,
    VerificationMethod::QrCodeShowV1,
];

async fn maybe_autostart(request: &VerificationRequest) -> Result<(), matrix_sdk::Error> {
    if let Some(theirs) = request.their_supported_methods() {
        if theirs.contains(&VerificationMethod::QrCodeScanV1) &&
            theirs.contains(&VerificationMethod::ReciprocateV1)
        {
            // Generate a QR code to show. This doesn't actually mean we select
            // this flow.
            request.generate_qr_code().await?;
        } else if theirs.contains(&VerificationMethod::SasV1) {
            // We only have one method in common and don't need to query the
            // user.
            request.start_sas().await?;
        }
    }

    Ok(())
}

pub async fn handle_request(
    flow_id: String,
    other_user_id: OwnedUserId,
    other_device_id: OwnedDeviceId,
    client: Client,
    store: AsyncProgramStore,
) {
    let own_user_id = client.user_id().unwrap();
    let own_device_id = client.device_id().unwrap();
    if other_user_id == own_user_id && other_device_id == own_device_id {
        tracing::debug!("ignoring the verification request we sent");
        return;
    }

    let Some(request) = client
        .encryption()
        .get_verification_request(&other_user_id, &flow_id)
        .await
    else {
        tracing::warn!("couldn't find verification request in crypto store");
        return;
    };

    tracing::debug!("received a verification request");

    store.lock().await.application.verifications.insert(flow_id, request);
}

pub async fn handle_ready(
    flow_id: String,
    other_user_id: OwnedUserId,
    client: Client,
    store: AsyncProgramStore,
) {
    let Some(request) = client
        .encryption()
        .get_verification_request(&other_user_id, &flow_id)
        .await
    else {
        tracing::warn!("couldn't find verification request in crypto store");
        return;
    };

    if let Err(err) = maybe_autostart(&request).await {
        tracing::warn!("unable to start verification process: {err}");
    }

    // Insert the request in case we missed it. Not sure if this is needed.
    // Might happen with room verification requests if the client is restarted.
    store.lock().await.application.verifications.insert(flow_id, request);
}

pub async fn handle_start(flow_id: String, other_user_id: OwnedUserId, client: Client) {
    match client.encryption().get_verification(&other_user_id, &flow_id).await {
        Some(Verification::SasV1(sas)) => {
            tracing::debug!("accepting SAS verification flow");
            if let Err(err) = sas.accept().await {
                tracing::warn!("unable to accept SAS verification flow: {err}");
            }
        },
        Some(_) => {
            tracing::info!("ignoring verification start with unsupported method");
        },
        None => {
            tracing::warn!("couldn't find verification request in crypto store");
        },
    }
}

pub async fn iamb_verify(
    act: VerifyAction,
    flow_id: String,
    store: &ProgramStore,
) -> IambResult<EditInfo> {
    let Some(request) = store.application.verifications.get(&flow_id) else {
        return Err(IambError::InvalidVerificationId(flow_id).into());
    };

    match act {
        VerifyAction::Accept => {
            if request.their_supported_methods().is_none_or(|theirs| {
                !(theirs.contains(&VerificationMethod::SasV1) ||
                    theirs.contains(&VerificationMethod::QrCodeScanV1) &&
                        theirs.contains(&VerificationMethod::ReciprocateV1))
            }) {
                let msg = "We don't have any verification methods in common.";
                let err = UIError::Failure(msg.into());
                return Err(err);
            }

            request
                .accept_with_methods(SUPPORTED_METHODS.into())
                .await
                .map_err(IambError::from)?;

            maybe_autostart(request).await.map_err(IambError::from)?;

            Ok(Some(InfoMessage::from("Accepted verification request")))
        },
        VerifyAction::Emoji => {
            if request
                .their_supported_methods()
                .is_none_or(|theirs| !theirs.contains(&VerificationMethod::SasV1))
            {
                let msg = "The other party doesn't support emoji verification.";
                let err = UIError::Failure(msg.into());
                return Err(err);
            }

            if request.start_sas().await.map_err(IambError::from)?.is_some() {
                Ok(Some(InfoMessage::from("Verification started")))
            } else {
                let msg = "Can't start interactive verification at this point.";
                let err = UIError::Failure(msg.into());
                Err(err)
            }
        },
        VerifyAction::Cancel => {
            request.cancel().await.map_err(IambError::from)?;
            Ok(Some(InfoMessage::from("Cancelled verification")))
        },
        VerifyAction::Confirm => {
            match request.state() {
                VerificationRequestState::Transitioned {
                    verification: Verification::SasV1(sas),
                } if sas.can_be_presented() => {
                    sas.confirm().await.map_err(IambError::from)?;
                },
                VerificationRequestState::Transitioned { verification: Verification::QrV1(qr) }
                    if qr.has_been_scanned() =>
                {
                    qr.confirm().await.map_err(IambError::from)?;
                },
                _ => {
                    let msg = "Can only confirm in-progress verifications!";
                    let err = UIError::Failure(msg.into());
                    return Err(err);
                },
            }

            Ok(Some(InfoMessage::from("Confirmed verification")))
        },
        VerifyAction::Mismatch => {
            match request.state() {
                VerificationRequestState::Transitioned {
                    verification: Verification::SasV1(sas),
                } if sas.can_be_presented() => {
                    sas.mismatch().await.map_err(IambError::from)?;
                },
                VerificationRequestState::Transitioned { verification: Verification::QrV1(qr) }
                    if qr.has_been_scanned() =>
                {
                    qr.cancel().await.map_err(IambError::from)?;
                },
                _ => {
                    let msg = "Can only reject in-progress verifications!";
                    let err = UIError::Failure(msg.into());
                    return Err(err);
                },
            }

            Ok(Some(InfoMessage::from("Rejected verification")))
        },
    }
}

pub async fn iamb_verify_request(
    user_id: &UserId,
    store: &mut ProgramStore,
) -> IambResult<EditInfo> {
    let enc = store.application.worker.client.encryption();

    let Some(identity) = enc.get_user_identity(user_id).await.map_err(IambError::from)? else {
        let msg = format!("Could not find identity information for {user_id}");
        let err = UIError::Failure(msg);
        return Err(err);
    };

    let request = identity.request_verification_with_methods(SUPPORTED_METHODS.into());
    let request = request.await.map_err(IambError::from)?;

    let flow_id = request.flow_id().to_owned();
    store.application.verifications.insert(flow_id, request);

    let info = format!("Sent verification request to {user_id}");
    Ok(Some(InfoMessage::from(info)))
}
