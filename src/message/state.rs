//! Code for displaying state events.
use std::borrow::Cow;
use std::str::FromStr;

use matrix_sdk::ruma::{
    OwnedRoomId,
    UserId,
    events::{
        AnyStateEventContentChange,
        AnySyncStateEvent,
        StateEventContentChange,
        room::member::MembershipChange,
    },
};

use crate::message::TreeGenState;

use super::html::{StyleTree, StyleTreeNode};
use ratatui::style::{Modifier as StyleModifier, Style};

fn bold(s: impl Into<Cow<'static, str>>) -> StyleTreeNode {
    let bold = Style::default().add_modifier(StyleModifier::BOLD);
    let text = StyleTreeNode::Text(s.into());
    StyleTreeNode::Style(Box::new(text), bold)
}

pub fn body_cow_state(ev: &AnySyncStateEvent) -> Cow<'static, str> {
    let event = match ev.content_change() {
        AnyStateEventContentChange::PolicyRuleRoom(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let mut m = format!(
                "* updated the room policy rule for {:?} to {:?}",
                content.0.entity,
                content.0.recommendation.as_str()
            );

            if !content.0.reason.is_empty() {
                m.push_str(" (reason: ");
                m.push_str(&content.0.reason);
                m.push(')');
            }

            m
        },
        AnyStateEventContentChange::PolicyRuleServer(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let mut m = format!(
                "* updated the server policy rule for {:?} to {:?}",
                content.0.entity,
                content.0.recommendation.as_str()
            );

            if !content.0.reason.is_empty() {
                m.push_str(" (reason: ");
                m.push_str(&content.0.reason);
                m.push(')');
            }

            m
        },
        AnyStateEventContentChange::PolicyRuleUser(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let mut m = format!(
                "* updated the user policy rule for {:?} to {:?}",
                content.0.entity,
                content.0.recommendation.as_str()
            );

            if !content.0.reason.is_empty() {
                m.push_str(" (reason: ");
                m.push_str(&content.0.reason);
                m.push(')');
            }

            m
        },
        AnyStateEventContentChange::RoomAvatar(StateEventContentChange::Original {
            content,
            prev_content,
        }) => {
            let prev_url = prev_content.as_ref().and_then(|p| p.url.as_ref());

            match (prev_url, content.url) {
                (None, Some(_)) => return Cow::Borrowed("* added a room avatar"),
                (Some(old), Some(new)) => {
                    if old != &new {
                        return Cow::Borrowed("* replaced the room avatar");
                    }

                    return Cow::Borrowed("* updated the room avatar state");
                },
                (Some(_), None) => return Cow::Borrowed("* removed the room avatar"),
                (None, None) => return Cow::Borrowed("* updated the room avatar state"),
            }
        },
        AnyStateEventContentChange::RoomCanonicalAlias(StateEventContentChange::Original {
            content,
            prev_content,
        }) => {
            let old_canon = prev_content.as_ref().and_then(|p| p.alias.as_ref());
            let new_canon = content.alias.as_ref();

            match (old_canon, new_canon) {
                (None, Some(canon)) => {
                    format!("* updated the canonical alias for the room to: {canon}")
                },
                (Some(old), Some(new)) => {
                    if old != new {
                        format!("* updated the canonical alias for the room to: {new}")
                    } else {
                        return Cow::Borrowed("* removed the canonical alias for the room");
                    }
                },
                (Some(_), None) => {
                    return Cow::Borrowed("* removed the canonical alias for the room");
                },
                (None, None) => {
                    return Cow::Borrowed("* did not change the canonical alias");
                },
            }
        },
        AnyStateEventContentChange::RoomCreate(StateEventContentChange::Original {
            content,
            ..
        }) => {
            if content.federate {
                return Cow::Borrowed("* created a federated room");
            } else {
                return Cow::Borrowed("* created a non-federated room");
            }
        },
        AnyStateEventContentChange::RoomEncryption(StateEventContentChange::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the encryption settings for the room");
        },
        AnyStateEventContentChange::RoomGuestAccess(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!("* set guest access for the room to {:?}", content.guest_access.as_str())
        },
        AnyStateEventContentChange::RoomHistoryVisibility(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!(
                "* updated history visibility for the room to {:?}",
                content.history_visibility.as_str()
            )
        },
        AnyStateEventContentChange::RoomJoinRules(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!("* update the join rules for the room to {:?}", content.join_rule.as_str())
        },
        AnyStateEventContentChange::RoomMember(StateEventContentChange::Original {
            content,
            prev_content,
        }) => {
            let Ok(state_key) = UserId::parse(ev.state_key()) else {
                return Cow::Owned(format!(
                    "* failed to calculate membership change for {:?}",
                    ev.state_key()
                ));
            };

            let prev_details = prev_content.as_ref().map(|p| p.details());
            let change = content.membership_change(prev_details, ev.sender(), &state_key);

            match change {
                MembershipChange::None => {
                    format!("* did nothing to {state_key}")
                },
                MembershipChange::Error => {
                    format!("* failed to calculate membership change to {state_key}")
                },
                MembershipChange::Joined => {
                    return Cow::Borrowed("* joined the room");
                },
                MembershipChange::Left => {
                    return Cow::Borrowed("* left the room");
                },
                MembershipChange::Banned => {
                    format!("* banned {state_key} from the room")
                },
                MembershipChange::Unbanned => {
                    format!("* unbanned {state_key} from the room")
                },
                MembershipChange::Kicked => {
                    format!("* kicked {state_key} from the room")
                },
                MembershipChange::Invited => {
                    format!("* invited {state_key} to the room")
                },
                MembershipChange::KickedAndBanned => {
                    format!("* kicked and banned {state_key} from the room")
                },
                MembershipChange::InvitationAccepted => {
                    return Cow::Borrowed("* accepted an invitation to join the room");
                },
                MembershipChange::InvitationRejected => {
                    return Cow::Borrowed("* rejected an invitation to join the room");
                },
                MembershipChange::InvitationRevoked => {
                    format!("* revoked an invitation for {state_key} to join the room")
                },
                MembershipChange::Knocked => {
                    return Cow::Borrowed("* would like to join the room");
                },
                MembershipChange::KnockAccepted => {
                    format!("* accepted the room knock from {state_key}")
                },
                MembershipChange::KnockRetracted => {
                    return Cow::Borrowed("* retracted their room knock");
                },
                MembershipChange::KnockDenied => {
                    format!("* rejected the room knock from {state_key}")
                },
                MembershipChange::ProfileChanged { displayname_change, avatar_url_change } => {
                    match (displayname_change, avatar_url_change) {
                        (Some(change), avatar_change) => {
                            let mut m = match (change.old, change.new) {
                                (None, Some(new)) => {
                                    format!("* set their display name to {new:?}")
                                },
                                (Some(old), Some(new)) => {
                                    format!("* changed their display name from {old} to {new}")
                                },
                                (Some(_), None) => "* unset their display name".to_string(),
                                (None, None) => {
                                    "* made an unknown change to their display name".to_string()
                                },
                            };

                            if avatar_change.is_some() {
                                m.push_str(" and changed their user avatar");
                            }

                            m
                        },
                        (None, Some(change)) => {
                            match (change.old, change.new) {
                                (None, Some(_)) => {
                                    return Cow::Borrowed("* added a user avatar");
                                },
                                (Some(_), Some(_)) => {
                                    return Cow::Borrowed("* changed their user avatar");
                                },
                                (Some(_), None) => {
                                    return Cow::Borrowed("* removed their user avatar");
                                },
                                (None, None) => {
                                    return Cow::Borrowed(
                                        "* made an unknown change to their user avatar",
                                    );
                                },
                            }
                        },
                        (None, None) => {
                            return Cow::Borrowed("* changed their user profile");
                        },
                    }
                },
                ev => {
                    format!("* made an unknown membership change to {state_key}: {ev:?}")
                },
            }
        },
        AnyStateEventContentChange::RoomName(StateEventContentChange::Original {
            content, ..
        }) => {
            format!("* updated the room name to {:?}", content.name)
        },
        AnyStateEventContentChange::RoomPinnedEvents(StateEventContentChange::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the pinned events for the room");
        },
        AnyStateEventContentChange::RoomPolicy(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!("* updated the room policy server to {}", content.via)
        },
        AnyStateEventContentChange::RoomPowerLevels(StateEventContentChange::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the power levels for the room");
        },
        AnyStateEventContentChange::RoomServerAcl(StateEventContentChange::Original { .. }) => {
            return Cow::Borrowed("* updated the room's server ACLs");
        },
        AnyStateEventContentChange::RoomThirdPartyInvite(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!("* sent a third-party invite to {:?}", content.display_name)
        },
        AnyStateEventContentChange::RoomTombstone(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!(
                "* upgraded the room; replacement room is {}",
                content.replacement_room.as_str()
            )
        },
        AnyStateEventContentChange::RoomTopic(StateEventContentChange::Original {
            content,
            ..
        }) => {
            format!("* set the room topic to {:?}", content.topic)
        },
        AnyStateEventContentChange::SpaceChild(StateEventContentChange::Original { .. }) => {
            format!("* added a space child: {}", ev.state_key())
        },
        AnyStateEventContentChange::SpaceParent(StateEventContentChange::Original {
            content,
            ..
        }) => {
            if content.canonical {
                format!("* added a canonical parent space: {}", ev.state_key())
            } else {
                format!("* added a parent space: {}", ev.state_key())
            }
        },
        AnyStateEventContentChange::BeaconInfo(StateEventContentChange::Original { .. }) => {
            return Cow::Borrowed("* shared beacon information");
        },
        AnyStateEventContentChange::CallMember(StateEventContentChange::Original { .. }) => {
            return Cow::Borrowed("* updated membership for room call");
        },
        AnyStateEventContentChange::MemberHints(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let mut m = String::from("* updated the list of service members in the room hints: ");

            for (i, member) in content.service_members.iter().enumerate() {
                if i != 0 {
                    m.push_str(", ");
                }

                m.push_str(member.as_str());
            }

            m
        },

        // Redacted variants of state events:
        AnyStateEventContentChange::PolicyRuleRoom(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated a room policy rule (redacted)");
        },
        AnyStateEventContentChange::PolicyRuleServer(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated a server policy rule (redacted)");
        },
        AnyStateEventContentChange::PolicyRuleUser(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated a user policy rule (redacted)");
        },
        AnyStateEventContentChange::RoomAvatar(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room avatar (redacted)");
        },
        AnyStateEventContentChange::RoomCanonicalAlias(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the canonical alias for the room (redacted)");
        },
        AnyStateEventContentChange::RoomCreate(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* created the room (redacted)");
        },
        AnyStateEventContentChange::RoomEncryption(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the encryption settings for the room (redacted)");
        },
        AnyStateEventContentChange::RoomGuestAccess(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed(
                "* updated the guest access configuration for the room (redacted)",
            );
        },
        AnyStateEventContentChange::RoomHistoryVisibility(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated history visilibity for the room (redacted)");
        },
        AnyStateEventContentChange::RoomJoinRules(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the join rules for the room (redacted)");
        },
        AnyStateEventContentChange::RoomMember(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room membership (redacted)");
        },
        AnyStateEventContentChange::RoomName(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room name (redacted)");
        },
        AnyStateEventContentChange::RoomPinnedEvents(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the pinned events for the room (redacted)");
        },
        AnyStateEventContentChange::RoomPolicy(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room policy server (redacted)");
        },
        AnyStateEventContentChange::RoomPowerLevels(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the power levels for the room (redacted)");
        },
        AnyStateEventContentChange::RoomServerAcl(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room's server ACLs (redacted)");
        },
        AnyStateEventContentChange::RoomThirdPartyInvite(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* sent a third-party invite (redacted)");
        },
        AnyStateEventContentChange::RoomTombstone(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* upgraded the room (redacted)");
        },
        AnyStateEventContentChange::RoomTopic(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* updated the room topic (redacted)");
        },
        AnyStateEventContentChange::SpaceChild(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* added a space child (redacted)");
        },
        AnyStateEventContentChange::SpaceParent(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* added a parent space (redacted)");
        },
        AnyStateEventContentChange::BeaconInfo(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("* shared beacon information (redacted)");
        },
        AnyStateEventContentChange::CallMember(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("Call membership changed");
        },
        AnyStateEventContentChange::MemberHints(StateEventContentChange::Redacted(_)) => {
            return Cow::Borrowed("Member hints changed");
        },

        // Handle unknown events:
        e => {
            format!("* sent an unknown state event: {:?}", e.event_type())
        },
    };

    return Cow::Owned(event);
}

pub fn html_state(ev: &AnySyncStateEvent) -> StyleTree {
    let children = match ev.content_change() {
        AnyStateEventContentChange::PolicyRuleRoom(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the room policy rule for ".into());
            let entity = bold(format!("{:?}", content.0.entity));
            let middle = StyleTreeNode::Text(" to ".into());
            let rec =
                StyleTreeNode::Text(format!("{:?}", content.0.recommendation.as_str()).into());
            let mut cs = vec![prefix, entity, middle, rec];

            if !content.0.reason.is_empty() {
                let reason = format!(" (reason: {})", content.0.reason);
                cs.push(StyleTreeNode::Text(reason.into()));
            }

            cs
        },
        AnyStateEventContentChange::PolicyRuleServer(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the server policy rule for ".into());
            let entity = bold(format!("{:?}", content.0.entity));
            let middle = StyleTreeNode::Text(" to ".into());
            let rec =
                StyleTreeNode::Text(format!("{:?}", content.0.recommendation.as_str()).into());
            let mut cs = vec![prefix, entity, middle, rec];

            if !content.0.reason.is_empty() {
                let reason = format!(" (reason: {})", content.0.reason);
                cs.push(StyleTreeNode::Text(reason.into()));
            }

            cs
        },
        AnyStateEventContentChange::PolicyRuleUser(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the user policy rule for ".into());
            let entity = bold(format!("{:?}", content.0.entity));
            let middle = StyleTreeNode::Text(" to ".into());
            let rec =
                StyleTreeNode::Text(format!("{:?}", content.0.recommendation.as_str()).into());
            let mut cs = vec![prefix, entity, middle, rec];

            if !content.0.reason.is_empty() {
                let reason = format!(" (reason: {})", content.0.reason);
                cs.push(StyleTreeNode::Text(reason.into()));
            }

            cs
        },
        AnyStateEventContentChange::RoomAvatar(StateEventContentChange::Original {
            content,
            prev_content,
        }) => {
            let prev_url = prev_content.as_ref().and_then(|p| p.url.as_ref());

            let node = match (prev_url, content.url) {
                (None, Some(_)) => StyleTreeNode::Text("* added a room avatar".into()),
                (Some(old), Some(new)) => {
                    if old != &new {
                        StyleTreeNode::Text("* replaced the room avatar".into())
                    } else {
                        StyleTreeNode::Text("* updated the room avatar state".into())
                    }
                },
                (Some(_), None) => StyleTreeNode::Text("* removed the room avatar".into()),
                (None, None) => StyleTreeNode::Text("* updated the room avatar state".into()),
            };

            vec![node]
        },
        AnyStateEventContentChange::RoomCanonicalAlias(StateEventContentChange::Original {
            content,
            ..
        }) => {
            if let Some(canon) = content.alias.as_ref() {
                let canon = StyleTreeNode::RoomAlias(canon.to_owned(), Some('0'));
                let prefix =
                    StyleTreeNode::Text("* updated the canonical alias for the room to: ".into());
                vec![prefix, canon]
            } else {
                vec![StyleTreeNode::Text(
                    "* removed the canonical alias for the room".into(),
                )]
            }
        },
        AnyStateEventContentChange::RoomCreate(StateEventContentChange::Original {
            content,
            ..
        }) => {
            if content.federate {
                vec![StyleTreeNode::Text("* created a federated room".into())]
            } else {
                vec![StyleTreeNode::Text("* created a non-federated room".into())]
            }
        },
        AnyStateEventContentChange::RoomEncryption(StateEventContentChange::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room".into(),
            )]
        },
        AnyStateEventContentChange::RoomGuestAccess(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let access = bold(format!("{:?}", content.guest_access.as_str()));
            let prefix = StyleTreeNode::Text("* set guest access for the room to ".into());
            vec![prefix, access]
        },
        AnyStateEventContentChange::RoomHistoryVisibility(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix =
                StyleTreeNode::Text("* updated history visibility for the room to ".into());
            let vis = bold(format!("{:?}", content.history_visibility.as_str()));
            vec![prefix, vis]
        },
        AnyStateEventContentChange::RoomJoinRules(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* update the join rules for the room to ".into());
            let rule = bold(format!("{:?}", content.join_rule.as_str()));
            vec![prefix, rule]
        },
        AnyStateEventContentChange::RoomMember(StateEventContentChange::Original {
            content,
            prev_content,
        }) => {
            let Ok(state_key) = UserId::parse(ev.state_key()) else {
                let prefix =
                    StyleTreeNode::Text("* failed to calculate membership change for ".into());
                let user_id = bold(format!("{:?}", ev.state_key()));
                let children = vec![prefix, user_id];

                return StyleTree { children };
            };

            let prev_details = prev_content.as_ref().map(|p| p.details());
            let change = content.membership_change(prev_details, ev.sender(), &state_key);
            let user_id = StyleTreeNode::UserId(state_key.clone(), Some('0'));

            match change {
                MembershipChange::None => {
                    let prefix = StyleTreeNode::Text("* did nothing to ".into());
                    vec![prefix, user_id]
                },
                MembershipChange::Error => {
                    let prefix =
                        StyleTreeNode::Text("* failed to calculate membership change to ".into());
                    vec![prefix, user_id]
                },
                MembershipChange::Joined => {
                    vec![StyleTreeNode::Text("* joined the room".into())]
                },
                MembershipChange::Left => {
                    vec![StyleTreeNode::Text("* left the room".into())]
                },
                MembershipChange::Banned => {
                    let prefix = StyleTreeNode::Text("* banned ".into());
                    let suffix = StyleTreeNode::Text(" from the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::Unbanned => {
                    let prefix = StyleTreeNode::Text("* unbanned ".into());
                    let suffix = StyleTreeNode::Text(" from the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::Kicked => {
                    let prefix = StyleTreeNode::Text("* kicked ".into());
                    let suffix = StyleTreeNode::Text(" from the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::Invited => {
                    let prefix = StyleTreeNode::Text("* invited ".into());
                    let suffix = StyleTreeNode::Text(" to the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::KickedAndBanned => {
                    let prefix = StyleTreeNode::Text("* kicked and banned ".into());
                    let suffix = StyleTreeNode::Text(" from the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::InvitationAccepted => {
                    vec![StyleTreeNode::Text(
                        "* accepted an invitation to join the room".into(),
                    )]
                },
                MembershipChange::InvitationRejected => {
                    vec![StyleTreeNode::Text(
                        "* rejected an invitation to join the room".into(),
                    )]
                },
                MembershipChange::InvitationRevoked => {
                    let prefix = StyleTreeNode::Text("* revoked an invitation for ".into());
                    let suffix = StyleTreeNode::Text(" to join the room".into());
                    vec![prefix, user_id, suffix]
                },
                MembershipChange::Knocked => {
                    vec![StyleTreeNode::Text("* would like to join the room".into())]
                },
                MembershipChange::KnockAccepted => {
                    let prefix = StyleTreeNode::Text("* accepted the room knock from ".into());
                    vec![prefix, user_id]
                },
                MembershipChange::KnockRetracted => {
                    vec![StyleTreeNode::Text("* retracted their room knock".into())]
                },
                MembershipChange::KnockDenied => {
                    let prefix = StyleTreeNode::Text("* rejected the room knock from ".into());
                    vec![prefix, user_id]
                },
                MembershipChange::ProfileChanged { displayname_change, avatar_url_change } => {
                    match (displayname_change, avatar_url_change) {
                        (Some(change), avatar_change) => {
                            let mut m = match (change.old, change.new) {
                                (None, Some(new)) => {
                                    vec![
                                        StyleTreeNode::Text("* set their display name to ".into()),
                                        StyleTreeNode::DisplayName(
                                            new.into(),
                                            state_key,
                                            Some('0'),
                                        ),
                                    ]
                                },
                                (Some(old), Some(new)) => {
                                    vec![
                                        StyleTreeNode::Text(
                                            "* changed their display name from ".into(),
                                        ),
                                        StyleTreeNode::DisplayName(
                                            old.into(),
                                            state_key.clone(),
                                            Some('0'),
                                        ),
                                        StyleTreeNode::Text(" to ".into()),
                                        StyleTreeNode::DisplayName(new.into(), state_key, None),
                                    ]
                                },
                                (Some(_), None) => {
                                    vec![StyleTreeNode::Text("* unset their display name".into())]
                                },
                                (None, None) => {
                                    vec![StyleTreeNode::Text(
                                        "* made an unknown change to their display name".into(),
                                    )]
                                },
                            };

                            if avatar_change.is_some() {
                                m.push(StyleTreeNode::Text(
                                    " and changed their user avatar".into(),
                                ));
                            }

                            m
                        },
                        (None, Some(change)) => {
                            let m = match (change.old, change.new) {
                                (None, Some(_)) => Cow::Borrowed("* added a user avatar"),
                                (Some(_), Some(_)) => Cow::Borrowed("* changed their user avatar"),
                                (Some(_), None) => Cow::Borrowed("* removed their user avatar"),
                                (None, None) => {
                                    Cow::Borrowed("* made an unknown change to their user avatar")
                                },
                            };

                            vec![StyleTreeNode::Text(m)]
                        },
                        (None, None) => {
                            vec![StyleTreeNode::Text("* changed their user profile".into())]
                        },
                    }
                },
                ev => {
                    let prefix =
                        StyleTreeNode::Text("* made an unknown membership change to ".into());
                    let suffix = StyleTreeNode::Text(format!(": {ev:?}").into());
                    vec![prefix, user_id, suffix]
                },
            }
        },
        AnyStateEventContentChange::RoomName(StateEventContentChange::Original {
            content, ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the room name to ".into());
            let name = bold(format!("{:?}", content.name));
            vec![prefix, name]
        },
        AnyStateEventContentChange::RoomPinnedEvents(StateEventContentChange::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room".into(),
            )]
        },
        AnyStateEventContentChange::RoomPolicy(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the room policy server to ".into());
            let server = bold(format!("{}", content.via));
            vec![prefix, server]
        },
        AnyStateEventContentChange::RoomPowerLevels(StateEventContentChange::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room".into(),
            )]
        },
        AnyStateEventContentChange::RoomServerAcl(StateEventContentChange::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs".into(),
            )]
        },
        AnyStateEventContentChange::RoomThirdPartyInvite(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* sent a third-party invite to ".into());
            let name = bold(format!("{:?}", content.display_name));
            vec![prefix, name]
        },
        AnyStateEventContentChange::RoomTombstone(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* upgraded the room; replacement room is ".into());
            let room = StyleTreeNode::RoomId(content.replacement_room.clone(), vec![], Some('0'));
            vec![prefix, room]
        },
        AnyStateEventContentChange::RoomTopic(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* set the room topic to ".into());
            let topic = bold(format!("{:?}", content.topic));
            vec![prefix, topic]
        },
        AnyStateEventContentChange::SpaceChild(StateEventContentChange::Original { .. }) => {
            let prefix = StyleTreeNode::Text("* added a space child: ".into());

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(ev.state_key()) {
                StyleTreeNode::RoomId(room_id, vec![], Some('0'))
            } else {
                bold(ev.state_key().to_string())
            };

            vec![prefix, room_id]
        },
        AnyStateEventContentChange::SpaceParent(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = if content.canonical {
                StyleTreeNode::Text("* added a canonical parent space: ".into())
            } else {
                StyleTreeNode::Text("* added a parent space: ".into())
            };

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(ev.state_key()) {
                StyleTreeNode::RoomId(room_id, vec![], Some('0'))
            } else {
                bold(ev.state_key().to_string())
            };

            vec![prefix, room_id]
        },
        AnyStateEventContentChange::BeaconInfo(StateEventContentChange::Original { .. }) => {
            vec![StyleTreeNode::Text("* shared beacon information".into())]
        },
        AnyStateEventContentChange::CallMember(StateEventContentChange::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated membership for room call".into(),
            )]
        },
        AnyStateEventContentChange::MemberHints(StateEventContentChange::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text(
                "* updated the list of service members in the room hints: ".into(),
            );
            let mut cs = vec![prefix];

            let mut state = TreeGenState { link_num: 0 };

            for (i, member) in content.service_members.iter().enumerate() {
                if i != 0 {
                    cs.push(StyleTreeNode::Text(", ".into()));
                }

                let c = state.next_link_char();

                cs.push(StyleTreeNode::UserId(member.clone(), c));
            }

            cs
        },

        // Redacted variants of state events:
        AnyStateEventContentChange::PolicyRuleRoom(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a room policy rule (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::PolicyRuleServer(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a server policy rule (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::PolicyRuleUser(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a user policy rule (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomAvatar(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room avatar (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomCanonicalAlias(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the canonical alias for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomCreate(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text("* created the room (redacted)".into())]
        },
        AnyStateEventContentChange::RoomEncryption(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomGuestAccess(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the guest access configuration for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomHistoryVisibility(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated history visilibity for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomJoinRules(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the join rules for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomMember(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room membership (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomName(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room name (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomPinnedEvents(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomPolicy(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room policy server (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomPowerLevels(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomServerAcl(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomThirdPartyInvite(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* sent a third-party invite (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::RoomTombstone(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text("* upgraded the room (redacted)".into())]
        },
        AnyStateEventContentChange::RoomTopic(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room topic (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::SpaceChild(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a space child (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::SpaceParent(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a parent space (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::BeaconInfo(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* shared beacon information (redacted)".into(),
            )]
        },
        AnyStateEventContentChange::CallMember(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text("Call membership changed".into())]
        },
        AnyStateEventContentChange::MemberHints(StateEventContentChange::Redacted(_)) => {
            vec![StyleTreeNode::Text("Member hints changed".into())]
        },

        // Handle unknown events:
        e => {
            let prefix = StyleTreeNode::Text("* sent an unknown state event: ".into());
            let event = bold(format!("{:?}", e.event_type()));
            vec![prefix, event]
        },
    };

    StyleTree { children }
}
