//! Code for displaying state events.
use std::borrow::Cow;
use std::str::FromStr;

use matrix_sdk::ruma::{
    events::{
        room::member::MembershipChange,
        AnyFullStateEventContent,
        AnySyncStateEvent,
        FullStateEventContent,
    },
    OwnedRoomId,
    UserId,
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
    let event = match ev.content() {
        AnyFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Original {
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
        AnyFullStateEventContent::PolicyRuleServer(FullStateEventContent::Original {
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
        AnyFullStateEventContent::PolicyRuleUser(FullStateEventContent::Original {
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
        AnyFullStateEventContent::RoomAliases(FullStateEventContent::Original {
            content, ..
        }) => {
            let mut m = String::from("* set the room aliases to: ");

            for (i, alias) in content.aliases.iter().enumerate() {
                if i != 0 {
                    m.push_str(", ");
                }

                m.push_str(alias.as_str());
            }

            m
        },
        AnyFullStateEventContent::RoomAvatar(FullStateEventContent::Original {
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
        AnyFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Original {
            content,
            prev_content,
        }) => {
            let old_canon = prev_content.as_ref().and_then(|p| p.alias.as_ref());
            let new_canon = content.alias.as_ref();

            match (old_canon, new_canon) {
                (None, Some(canon)) => {
                    format!("* updated the canonical alias for the room to: {}", canon)
                },
                (Some(old), Some(new)) => {
                    if old != new {
                        format!("* updated the canonical alias for the room to: {}", new)
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
        AnyFullStateEventContent::RoomCreate(FullStateEventContent::Original {
            content, ..
        }) => {
            if content.federate {
                return Cow::Borrowed("* created a federated room");
            } else {
                return Cow::Borrowed("* created a non-federated room");
            }
        },
        AnyFullStateEventContent::RoomEncryption(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* updated the encryption settings for the room");
        },
        AnyFullStateEventContent::RoomGuestAccess(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* set guest access for the room to {:?}", content.guest_access.as_str())
        },
        AnyFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!(
                "* updated history visibility for the room to {:?}",
                content.history_visibility.as_str()
            )
        },
        AnyFullStateEventContent::RoomJoinRules(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* update the join rules for the room to {:?}", content.join_rule.as_str())
        },
        AnyFullStateEventContent::RoomMember(FullStateEventContent::Original {
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
                    format!("* did nothing to {}", state_key)
                },
                MembershipChange::Error => {
                    format!("* failed to calculate membership change to {}", state_key)
                },
                MembershipChange::Joined => {
                    return Cow::Borrowed("* joined the room");
                },
                MembershipChange::Left => {
                    return Cow::Borrowed("* left the room");
                },
                MembershipChange::Banned => {
                    format!("* banned {} from the room", state_key)
                },
                MembershipChange::Unbanned => {
                    format!("* unbanned {} from the room", state_key)
                },
                MembershipChange::Kicked => {
                    format!("* kicked {} from the room", state_key)
                },
                MembershipChange::Invited => {
                    format!("* invited {} to the room", state_key)
                },
                MembershipChange::KickedAndBanned => {
                    format!("* kicked and banned {} from the room", state_key)
                },
                MembershipChange::InvitationAccepted => {
                    return Cow::Borrowed("* accepted an invitation to join the room");
                },
                MembershipChange::InvitationRejected => {
                    return Cow::Borrowed("* rejected an invitation to join the room");
                },
                MembershipChange::InvitationRevoked => {
                    format!("* revoked an invitation for {} to join the room", state_key)
                },
                MembershipChange::Knocked => {
                    return Cow::Borrowed("* would like to join the room");
                },
                MembershipChange::KnockAccepted => {
                    format!("* accepted the room knock from {}", state_key)
                },
                MembershipChange::KnockRetracted => {
                    return Cow::Borrowed("* retracted their room knock");
                },
                MembershipChange::KnockDenied => {
                    format!("* rejected the room knock from {}", state_key)
                },
                MembershipChange::ProfileChanged { displayname_change, avatar_url_change } => {
                    match (displayname_change, avatar_url_change) {
                        (Some(change), avatar_change) => {
                            let mut m = match (change.old, change.new) {
                                (None, Some(new)) => {
                                    format!("* set their display name to {:?}", new)
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
                    format!("* made an unknown membership change to {}: {:?}", state_key, ev)
                },
            }
        },
        AnyFullStateEventContent::RoomName(FullStateEventContent::Original { content, .. }) => {
            format!("* updated the room name to {:?}", content.name)
        },
        AnyFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* updated the pinned events for the room");
        },
        AnyFullStateEventContent::RoomPowerLevels(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* updated the power levels for the room");
        },
        AnyFullStateEventContent::RoomServerAcl(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* updated the room's server ACLs");
        },
        AnyFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* sent a third-party invite to {:?}", content.display_name)
        },
        AnyFullStateEventContent::RoomTombstone(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!(
                "* upgraded the room; replacement room is {}",
                content.replacement_room.as_str()
            )
        },
        AnyFullStateEventContent::RoomTopic(FullStateEventContent::Original {
            content, ..
        }) => {
            format!("* set the room topic to {:?}", content.topic)
        },
        AnyFullStateEventContent::SpaceChild(FullStateEventContent::Original { .. }) => {
            format!("* added a space child: {}", ev.state_key())
        },
        AnyFullStateEventContent::SpaceParent(FullStateEventContent::Original {
            content, ..
        }) => {
            if content.canonical {
                format!("* added a canonical parent space: {}", ev.state_key())
            } else {
                format!("* added a parent space: {}", ev.state_key())
            }
        },
        AnyFullStateEventContent::BeaconInfo(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* shared beacon information");
        },
        AnyFullStateEventContent::CallMember(FullStateEventContent::Original { .. }) => {
            return Cow::Borrowed("* updated membership for room call");
        },
        AnyFullStateEventContent::MemberHints(FullStateEventContent::Original {
            content, ..
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
        AnyFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a room policy rule (redacted)");
        },
        AnyFullStateEventContent::PolicyRuleServer(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a server policy rule (redacted)");
        },
        AnyFullStateEventContent::PolicyRuleUser(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a user policy rule (redacted)");
        },
        AnyFullStateEventContent::RoomAliases(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room aliases for the room (redacted)");
        },
        AnyFullStateEventContent::RoomAvatar(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room avatar (redacted)");
        },
        AnyFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the canonical alias for the room (redacted)");
        },
        AnyFullStateEventContent::RoomCreate(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* created the room (redacted)");
        },
        AnyFullStateEventContent::RoomEncryption(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the encryption settings for the room (redacted)");
        },
        AnyFullStateEventContent::RoomGuestAccess(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed(
                "* updated the guest access configuration for the room (redacted)",
            );
        },
        AnyFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated history visilibity for the room (redacted)");
        },
        AnyFullStateEventContent::RoomJoinRules(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the join rules for the room (redacted)");
        },
        AnyFullStateEventContent::RoomMember(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room membership (redacted)");
        },
        AnyFullStateEventContent::RoomName(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room name (redacted)");
        },
        AnyFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the pinned events for the room (redacted)");
        },
        AnyFullStateEventContent::RoomPowerLevels(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the power levels for the room (redacted)");
        },
        AnyFullStateEventContent::RoomServerAcl(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room's server ACLs (redacted)");
        },
        AnyFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* sent a third-party invite (redacted)");
        },
        AnyFullStateEventContent::RoomTombstone(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* upgraded the room (redacted)");
        },
        AnyFullStateEventContent::RoomTopic(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room topic (redacted)");
        },
        AnyFullStateEventContent::SpaceChild(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* added a space child (redacted)");
        },
        AnyFullStateEventContent::SpaceParent(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* added a parent space (redacted)");
        },
        AnyFullStateEventContent::BeaconInfo(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* shared beacon information (redacted)");
        },
        AnyFullStateEventContent::CallMember(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("Call membership changed");
        },
        AnyFullStateEventContent::MemberHints(FullStateEventContent::Redacted(_)) => {
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
    let children = match ev.content() {
        AnyFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Original {
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
        AnyFullStateEventContent::PolicyRuleServer(FullStateEventContent::Original {
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
        AnyFullStateEventContent::PolicyRuleUser(FullStateEventContent::Original {
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
        AnyFullStateEventContent::RoomAliases(FullStateEventContent::Original {
            content, ..
        }) => {
            let prefix = StyleTreeNode::Text("* set the room aliases to: ".into());
            let mut cs = vec![prefix];

            let mut state = TreeGenState { link_num: 0 };

            for (i, alias) in content.aliases.iter().enumerate() {
                if i != 0 {
                    cs.push(StyleTreeNode::Text(", ".into()));
                }

                let c = state.next_link_char();

                cs.push(StyleTreeNode::RoomAlias(alias.clone(), c));
            }

            cs
        },
        AnyFullStateEventContent::RoomAvatar(FullStateEventContent::Original {
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
        AnyFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Original {
            content,
            ..
        }) => {
            if let Some(canon) = content.alias.as_ref() {
                let canon = bold(canon.to_string());
                let prefix =
                    StyleTreeNode::Text("* updated the canonical alias for the room to: ".into());
                vec![prefix, canon]
            } else {
                vec![StyleTreeNode::Text(
                    "* removed the canonical alias for the room".into(),
                )]
            }
        },
        AnyFullStateEventContent::RoomCreate(FullStateEventContent::Original {
            content, ..
        }) => {
            if content.federate {
                vec![StyleTreeNode::Text("* created a federated room".into())]
            } else {
                vec![StyleTreeNode::Text("* created a non-federated room".into())]
            }
        },
        AnyFullStateEventContent::RoomEncryption(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room".into(),
            )]
        },
        AnyFullStateEventContent::RoomGuestAccess(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let access = bold(format!("{:?}", content.guest_access.as_str()));
            let prefix = StyleTreeNode::Text("* set guest access for the room to ".into());
            vec![prefix, access]
        },
        AnyFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix =
                StyleTreeNode::Text("* updated history visibility for the room to ".into());
            let vis = bold(format!("{:?}", content.history_visibility.as_str()));
            vec![prefix, vis]
        },
        AnyFullStateEventContent::RoomJoinRules(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* update the join rules for the room to ".into());
            let rule = bold(format!("{:?}", content.join_rule.as_str()));
            vec![prefix, rule]
        },
        AnyFullStateEventContent::RoomMember(FullStateEventContent::Original {
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
                    let suffix = StyleTreeNode::Text(format!(": {:?}", ev).into());
                    vec![prefix, user_id, suffix]
                },
            }
        },
        AnyFullStateEventContent::RoomName(FullStateEventContent::Original { content, .. }) => {
            let prefix = StyleTreeNode::Text("* updated the room name to ".into());
            let name = bold(format!("{:?}", content.name));
            vec![prefix, name]
        },
        AnyFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room".into(),
            )]
        },
        AnyFullStateEventContent::RoomPowerLevels(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room".into(),
            )]
        },
        AnyFullStateEventContent::RoomServerAcl(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs".into(),
            )]
        },
        AnyFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* sent a third-party invite to ".into());
            let name = bold(format!("{:?}", content.display_name));
            vec![prefix, name]
        },
        AnyFullStateEventContent::RoomTombstone(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* upgraded the room; replacement room is ".into());
            let room = StyleTreeNode::RoomId(content.replacement_room.clone(), Some('0'));
            vec![prefix, room]
        },
        AnyFullStateEventContent::RoomTopic(FullStateEventContent::Original {
            content, ..
        }) => {
            let prefix = StyleTreeNode::Text("* set the room topic to ".into());
            let topic = bold(format!("{:?}", content.topic));
            vec![prefix, topic]
        },
        AnyFullStateEventContent::SpaceChild(FullStateEventContent::Original { .. }) => {
            let prefix = StyleTreeNode::Text("* added a space child: ".into());

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(ev.state_key()) {
                StyleTreeNode::RoomId(room_id, Some('0'))
            } else {
                bold(ev.state_key().to_string())
            };

            vec![prefix, room_id]
        },
        AnyFullStateEventContent::SpaceParent(FullStateEventContent::Original {
            content, ..
        }) => {
            let prefix = if content.canonical {
                StyleTreeNode::Text("* added a canonical parent space: ".into())
            } else {
                StyleTreeNode::Text("* added a parent space: ".into())
            };

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(ev.state_key()) {
                StyleTreeNode::RoomId(room_id, Some('0'))
            } else {
                bold(ev.state_key().to_string())
            };

            vec![prefix, room_id]
        },
        AnyFullStateEventContent::BeaconInfo(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text("* shared beacon information".into())]
        },
        AnyFullStateEventContent::CallMember(FullStateEventContent::Original { .. }) => {
            vec![StyleTreeNode::Text(
                "* updated membership for room call".into(),
            )]
        },
        AnyFullStateEventContent::MemberHints(FullStateEventContent::Original {
            content, ..
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
        AnyFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a room policy rule (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::PolicyRuleServer(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a server policy rule (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::PolicyRuleUser(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a user policy rule (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomAliases(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room aliases for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomAvatar(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room avatar (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the canonical alias for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomCreate(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text("* created the room (redacted)".into())]
        },
        AnyFullStateEventContent::RoomEncryption(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomGuestAccess(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the guest access configuration for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated history visilibity for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomJoinRules(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the join rules for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomMember(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room membership (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomName(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room name (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomPowerLevels(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomServerAcl(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* sent a third-party invite (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::RoomTombstone(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text("* upgraded the room (redacted)".into())]
        },
        AnyFullStateEventContent::RoomTopic(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room topic (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::SpaceChild(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a space child (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::SpaceParent(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a parent space (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::BeaconInfo(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* shared beacon information (redacted)".into(),
            )]
        },
        AnyFullStateEventContent::CallMember(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text("Call membership changed".into())]
        },
        AnyFullStateEventContent::MemberHints(FullStateEventContent::Redacted(_)) => {
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
