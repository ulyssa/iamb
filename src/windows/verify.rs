use std::cmp::Ordering;
use std::fmt;

use matrix_sdk::encryption::verification::{
    Verification,
    VerificationRequest,
    VerificationRequestState,
};
use matrix_sdk::ruma::events::key::verification::VerificationMethod;
use matrix_sdk_crypto::matrix_sdk_qrcode::qrcode::render::unicode::Dense1x2;
use matrix_sdk_crypto::{QrVerificationState, SasState, format_emojis};

use modalkit::actions::{PromptAction, Promptable};
use modalkit::errors::{EditError, EditResult};
use modalkit::prelude::ViewportContext;
use modalkit_ratatui::list::{ListCursor, ListItem};
use ratatui::style::{Color, Modifier as StyleModifier, Style};
use ratatui::text::{Line, Span, Text};

use crate::base::{IambInfo, ProgramAction, ProgramContext, ProgramStore};

const BLACK_ON_WHITE: Style = Style::new().fg(Color::Black).bg(Color::White);

#[derive(Clone)]
pub struct VerifyItem {
    request: VerificationRequest,
    show_help: bool,
}

impl VerifyItem {
    pub fn new(request: VerificationRequest) -> Self {
        Self { request, show_help: false }
    }

    pub fn show_help(&mut self) {
        self.show_help = true;
    }
}

impl PartialEq for VerifyItem {
    fn eq(&self, other: &Self) -> bool {
        self.request.flow_id() == other.request.flow_id()
    }
}

impl Eq for VerifyItem {}

impl Ord for VerifyItem {
    fn cmp(&self, other: &Self) -> Ordering {
        fn state_val(req: &VerificationRequest) -> usize {
            // 0: running
            // 1: ready
            // 2: requests for this session
            // 3: all others
            // 4: canceled
            // 5: done
            match req.state() {
                VerificationRequestState::Requested { .. } => 2,
                VerificationRequestState::Ready { .. } => 1,
                VerificationRequestState::Transitioned {
                    verification: Verification::SasV1(sas),
                } => {
                    match sas.state() {
                        SasState::KeysExchanged { emojis: Some(_), .. } => 0,
                        SasState::Done { .. } => 5,
                        SasState::Cancelled(_) => 4,
                        _ => 3,
                    }
                },
                VerificationRequestState::Transitioned { verification: Verification::QrV1(qr) } => {
                    match qr.state() {
                        QrVerificationState::Started => 1,
                        QrVerificationState::Scanned => 0,
                        QrVerificationState::Done { .. } => 5,
                        QrVerificationState::Cancelled(_) => 4,
                        _ => 3,
                    }
                },
                VerificationRequestState::Done => 5,
                VerificationRequestState::Cancelled(_) => 4,
                _ => 3,
            }
        }

        fn device_val(req: &VerificationRequest) -> usize {
            if req.is_self_verification() { 1 } else { 2 }
        }

        let state1 = state_val(&self.request);
        let state2 = state_val(&other.request);

        let dev1 = device_val(&self.request);
        let dev2 = device_val(&other.request);

        let scmp = state1.cmp(&state2);
        let dcmp = dev1.cmp(&dev2);

        scmp.then(dcmp).then_with(|| {
            let did1 = self.request.flow_id();
            let did2 = other.request.flow_id();

            did1.cmp(did2)
        })
    }
}

impl PartialOrd for VerifyItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for VerifyItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.request.state() {
            VerificationRequestState::Requested { .. } => {
                write!(f, ":verify accept {}", self.request.flow_id())
            },
            VerificationRequestState::Ready { their_methods, .. }
                if their_methods.contains(&VerificationMethod::SasV1) =>
            {
                write!(f, ":verify emoji {}", self.request.flow_id())
            },
            VerificationRequestState::Transitioned { verification: Verification::SasV1(sas) } => {
                match sas.state() {
                    SasState::KeysExchanged { emojis: Some(_), .. } => {
                        write!(f, ":verify confirm {}", self.request.flow_id())
                    },
                    _ => Ok(()),
                }
            },
            VerificationRequestState::Transitioned { verification: Verification::QrV1(qr) } => {
                match qr.state() {
                    QrVerificationState::Started => {
                        write!(f, ":verify emoji {}", self.request.flow_id())
                    },
                    QrVerificationState::Scanned => {
                        write!(f, ":verify confirm {}", self.request.flow_id())
                    },
                    _ => Ok(()),
                }
            },
            _ => Ok(()),
        }
    }
}

impl ListItem<IambInfo> for VerifyItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        store: &mut ProgramStore,
    ) -> Text<'_> {
        let mut lines = vec![];
        let bold = Style::default().add_modifier(StyleModifier::BOLD);
        let selected_bold = super::selected_style(selected).add_modifier(StyleModifier::BOLD);
        let selected = super::selected_style(selected);

        let mut other_device = None;
        let state = match self.request.state() {
            _ if self.request.is_passive() => "completed with other device",
            VerificationRequestState::Created { .. } => "request sent",
            VerificationRequestState::Requested { their_methods, other_device_data } => {
                other_device = Some(other_device_data);

                if their_methods.contains(&VerificationMethod::SasV1) {
                    lines.push(Line::from("    To accept this request, run:"));
                    "requested"
                } else {
                    "no methods in common"
                }
            },
            VerificationRequestState::Ready { other_device_data, their_methods, .. } => {
                // This state should only be temporary since we either generate a qr code or
                // directly start SAS verification.

                other_device = Some(other_device_data);

                if their_methods.contains(&VerificationMethod::SasV1) {
                    lines.push(Line::from("    To start interactive verification, run:"));
                }

                "ready"
            },
            VerificationRequestState::Transitioned { verification: Verification::SasV1(sas) } => {
                other_device = Some(sas.other_device().to_owned());

                match sas.state() {
                    SasState::Created { .. } |
                    SasState::Started { .. } |
                    SasState::Accepted { .. } => "starting",
                    SasState::KeysExchanged { emojis: Some(emojis), .. } => {
                        lines.push(Line::from(
                            "    Both devices should see the following Emoji sequence:".to_string(),
                        ));
                        lines.push(Line::from(""));

                        for line in format_emojis(emojis.emojis).lines() {
                            lines.push(Line::from(format!("    {line}")));
                        }

                        lines.push(Line::from(""));
                        lines.push(Line::from("    If they don't match, run:"));
                        lines.push(Line::from(""));
                        lines.push(Line::from(Span::styled(
                            format!("        :verify mismatch {}", self.request.flow_id()),
                            bold,
                        )));
                        lines.push(Line::from(""));
                        lines.push(Line::from(
                            "    If everything looks right, you can confirm with:",
                        ));
                        "running"
                    },
                    SasState::KeysExchanged { emojis: None, .. } => "unsupported method",
                    SasState::Confirmed => "waiting for response",
                    SasState::Done { .. } => "done",
                    SasState::Cancelled(info) => {
                        lines.push(Line::from(format!("    Cancelled: {}", info.reason())));
                        "cancelled"
                    },
                }
            },
            VerificationRequestState::Transitioned { verification: Verification::QrV1(qr) } => {
                other_device = Some(qr.other_device().to_owned());

                match qr.state() {
                    QrVerificationState::Started => {
                        if let Ok(qrcode) = qr.to_qr_code() {
                            let rendered = qrcode.render::<Dense1x2>().build();
                            lines.extend(
                                rendered
                                    .lines()
                                    .map(|line| Line::styled(line.to_owned(), BLACK_ON_WHITE)),
                            );

                            lines.push(Line::from("    Scan this QR code with the other device."));
                            lines.push(Line::from(
                                "    To alternativly start interactive verification, run:",
                            ));
                        } else {
                            lines.push(Line::from("    To start interactive verification, run:"));
                        }

                        "ready"
                    },
                    QrVerificationState::Scanned => {
                        lines.push(Line::from(
                            "    Check whether the other device shows a successful verification."
                                .to_string(),
                        ));
                        lines.push(Line::from(""));
                        lines.push(Line::from("    If it shows an error, run:"));
                        lines.push(Line::from(""));
                        lines.push(Line::from(Span::styled(
                            format!("        :verify mismatch {}", self.request.flow_id()),
                            bold,
                        )));
                        lines.push(Line::from(""));
                        lines.push(Line::from(
                            "    If everything looks right, you can confirm with:",
                        ));
                        "running"
                    },
                    QrVerificationState::Confirmed => "waiting for response",
                    QrVerificationState::Reciprocated => {
                        tracing::error!(
                            "reached unreachable state of having scanned a verification QR code"
                        );
                        "inconsistent state"
                    },
                    QrVerificationState::Done { .. } => "done",
                    QrVerificationState::Cancelled(info) => {
                        lines.push(Line::from(format!("    Cancelled: {}", info.reason())));
                        "cancelled"
                    },
                }
            },
            VerificationRequestState::Transitioned { .. } => "unsupported method",
            VerificationRequestState::Done => "done",
            VerificationRequestState::Cancelled(info) => {
                lines.push(Line::from(format!("    Cancelled: {}", info.reason())));
                "cancelled"
            },
        };

        let line = if self.request.is_self_verification() {
            if let Some(device) = other_device {
                if let Some(display_name) = device.display_name() {
                    vec![
                        Span::styled("Device verification with ", selected),
                        Span::styled(display_name.to_owned(), selected_bold),
                        Span::styled(format!(" ({state})"), selected),
                    ]
                } else {
                    vec![
                        Span::styled("Device verification with ", selected),
                        Span::styled(device.device_id().to_string(), selected_bold),
                        Span::styled(format!(" ({state})"), selected),
                    ]
                }
            } else {
                vec![Span::styled(
                    format!("Device verification with any own device ({state})"),
                    selected,
                )]
            }
        } else {
            let color = store.application.settings.get_user_color(self.request.other_user_id());
            vec![
                Span::styled("User verification with ", selected),
                Span::styled(self.request.other_user_id().as_str(), selected_bold.patch(color)),
                Span::styled(format!(" ({state})"), selected),
            ]
        };
        lines.insert(0, line.into());

        let cmd = self.to_string();

        if !cmd.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(vec![Span::from("        "), Span::styled(cmd, bold)]));
            if self.show_help {
                lines.push(Line::from(""));
                lines.push(Line::from(vec![
                    Span::from("You can copy the above command with "),
                    Span::styled("yy", bold),
                    Span::from(" and then execute it with "),
                    Span::styled("@\"", bold),
                ]));
            }
        }

        Text::from(lines)
    }

    fn get_word(&self) -> Option<String> {
        None
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for VerifyItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => Ok(vec![]),
            PromptAction::Abort(_) => {
                let msg = "Cannot abort entry inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
        }
    }
}
