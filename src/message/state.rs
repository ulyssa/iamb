//! Code for displaying state events.
use std::borrow::Cow;
use std::str::FromStr;

use matrix_sdk::ruma::events::FullStateEventContent;
use matrix_sdk::ruma::OwnedRoomId;
use matrix_sdk_ui::timeline::{
    AnyOtherFullStateEventContent,
    MemberProfileChange,
    MembershipChange,
    OtherState,
    RoomMembershipChange,
};

use super::html::{StyleTree, StyleTreeNode};
use ratatui::style::{Modifier as StyleModifier, Style};

fn bold(s: impl Into<Cow<'static, str>>) -> StyleTreeNode {
    let bold = Style::default().add_modifier(StyleModifier::BOLD);
    let text = StyleTreeNode::Text(s.into());
    StyleTreeNode::Style(Box::new(text), bold)
}

pub fn body_cow_membership(change: &RoomMembershipChange) -> Cow<'static, str> {
    let user_id = change.user_id();
    let change = match change.change() {
        None => {
            format!("* changed {user_id} in unknown ways")
        },
        Some(MembershipChange::None) => {
            format!("* did nothing to {user_id}")
        },
        Some(MembershipChange::Error) => {
            format!("* failed to calculate membership change to {user_id}")
        },
        Some(MembershipChange::NotImplemented) => {
            format!("* changed {user_id} in unknown ways")
        },

        Some(MembershipChange::Joined) => {
            return Cow::Borrowed("* joined the room");
        },
        Some(MembershipChange::Left) => {
            return Cow::Borrowed("* left the room");
        },
        Some(MembershipChange::Banned) => {
            format!("* banned {user_id} from the room")
        },
        Some(MembershipChange::Unbanned) => {
            format!("* unbanned {user_id} from the room")
        },
        Some(MembershipChange::Kicked) => {
            format!("* kicked {user_id} from the room")
        },
        Some(MembershipChange::Invited) => {
            format!("* invited {user_id} to the room")
        },
        Some(MembershipChange::KickedAndBanned) => {
            format!("* kicked and banned {user_id} from the room")
        },
        Some(MembershipChange::InvitationAccepted) => {
            return Cow::Borrowed("* accepted an invitation to join the room");
        },
        Some(MembershipChange::InvitationRejected) => {
            return Cow::Borrowed("* rejected an invitation to join the room");
        },
        Some(MembershipChange::InvitationRevoked) => {
            format!("* revoked an invitation for {user_id} to join the room")
        },
        Some(MembershipChange::Knocked) => {
            return Cow::Borrowed("* would like to join the room");
        },
        Some(MembershipChange::KnockAccepted) => {
            format!("* accepted the room knock from {user_id}")
        },
        Some(MembershipChange::KnockRetracted) => {
            return Cow::Borrowed("* retracted their room knock");
        },
        Some(MembershipChange::KnockDenied) => {
            format!("* rejected the room knock from {user_id}")
        },
    };

    Cow::Owned(change)
}

pub fn body_cow_profile(change: &MemberProfileChange) -> Cow<'static, str> {
    match (change.displayname_change(), change.avatar_url_change()) {
        (Some(change), avatar_change) => {
            let mut m = match (&change.old, &change.new) {
                (None, Some(new)) => Cow::Owned(format!("* set their display name to {new:?}")),
                (Some(old), Some(new)) => {
                    Cow::Owned(format!("* changed their display name from {old} to {new}"))
                },
                (Some(_), None) => Cow::Borrowed("* unset their display name"),
                (None, None) => Cow::Borrowed("* made an unknown change to their display name"),
            };

            if avatar_change.is_some() {
                m.to_mut().push_str(" and changed their user avatar");
            }

            m
        },
        (None, Some(change)) => {
            match (&change.old, &change.new) {
                (None, Some(_)) => Cow::Borrowed("* added a user avatar"),
                (Some(_), Some(_)) => Cow::Borrowed("* changed their user avatar"),
                (Some(_), None) => Cow::Borrowed("* removed their user avatar"),
                (None, None) => Cow::Borrowed("* made an unknown change to their user avatar"),
            }
        },
        (None, None) => Cow::Borrowed("* changed their user profile"),
    }
}

pub fn body_cow_state(change: &OtherState) -> Cow<'static, str> {
    let event = match change.content() {
        AnyOtherFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::PolicyRuleServer(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::PolicyRuleUser(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::RoomAliases(FullStateEventContent::Original {
            content,
            ..
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
        AnyOtherFullStateEventContent::RoomAvatar(FullStateEventContent::Original {
            content,
            prev_content,
        }) => {
            let prev_url = prev_content.as_ref().and_then(|p| p.url.as_ref());

            match (prev_url, &content.url) {
                (None, Some(_)) => return Cow::Borrowed("* added a room avatar"),
                (Some(old), Some(new)) => {
                    if old != new {
                        return Cow::Borrowed("* replaced the room avatar");
                    }

                    return Cow::Borrowed("* updated the room avatar state");
                },
                (Some(_), None) => return Cow::Borrowed("* removed the room avatar"),
                (None, None) => return Cow::Borrowed("* updated the room avatar state"),
            }
        },
        AnyOtherFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::RoomCreate(FullStateEventContent::Original {
            content,
            ..
        }) => {
            if content.federate {
                return Cow::Borrowed("* created a federated room");
            } else {
                return Cow::Borrowed("* created a non-federated room");
            }
        },
        AnyOtherFullStateEventContent::RoomEncryption(FullStateEventContent::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the encryption settings for the room");
        },
        AnyOtherFullStateEventContent::RoomGuestAccess(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* set guest access for the room to {:?}", content.guest_access.as_str())
        },
        AnyOtherFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!(
                "* updated history visibility for the room to {:?}",
                content.history_visibility.as_str()
            )
        },
        AnyOtherFullStateEventContent::RoomJoinRules(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* update the join rules for the room to {:?}", content.join_rule.as_str())
        },
        AnyOtherFullStateEventContent::RoomName(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* updated the room name to {:?}", content.name)
        },
        AnyOtherFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the pinned events for the room");
        },
        AnyOtherFullStateEventContent::RoomPowerLevels(FullStateEventContent::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the power levels for the room");
        },
        AnyOtherFullStateEventContent::RoomServerAcl(FullStateEventContent::Original {
            ..
        }) => {
            return Cow::Borrowed("* updated the room's server ACLs");
        },
        AnyOtherFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* sent a third-party invite to {:?}", content.display_name)
        },
        AnyOtherFullStateEventContent::RoomTombstone(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!(
                "* upgraded the room; replacement room is {}",
                content.replacement_room.as_str()
            )
        },
        AnyOtherFullStateEventContent::RoomTopic(FullStateEventContent::Original {
            content,
            ..
        }) => {
            format!("* set the room topic to {:?}", content.topic)
        },
        AnyOtherFullStateEventContent::SpaceChild(FullStateEventContent::Original { .. }) => {
            format!("* added a space child: {}", change.state_key())
        },
        AnyOtherFullStateEventContent::SpaceParent(FullStateEventContent::Original {
            content,
            ..
        }) => {
            if content.canonical {
                format!("* added a canonical parent space: {}", change.state_key())
            } else {
                format!("* added a parent space: {}", change.state_key())
            }
        },

        // Redacted variants of state events:
        AnyOtherFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a room policy rule (redacted)");
        },
        AnyOtherFullStateEventContent::PolicyRuleServer(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a server policy rule (redacted)");
        },
        AnyOtherFullStateEventContent::PolicyRuleUser(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated a user policy rule (redacted)");
        },
        AnyOtherFullStateEventContent::RoomAliases(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room aliases for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomAvatar(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room avatar (redacted)");
        },
        AnyOtherFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the canonical alias for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomCreate(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* created the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomEncryption(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the encryption settings for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomGuestAccess(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed(
                "* updated the guest access configuration for the room (redacted)",
            );
        },
        AnyOtherFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Redacted(
            _,
        )) => {
            return Cow::Borrowed("* updated history visilibity for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomJoinRules(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the join rules for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomName(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room name (redacted)");
        },
        AnyOtherFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the pinned events for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomPowerLevels(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the power levels for the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomServerAcl(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room's server ACLs (redacted)");
        },
        AnyOtherFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* sent a third-party invite (redacted)");
        },
        AnyOtherFullStateEventContent::RoomTombstone(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* upgraded the room (redacted)");
        },
        AnyOtherFullStateEventContent::RoomTopic(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* updated the room topic (redacted)");
        },
        AnyOtherFullStateEventContent::SpaceChild(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* added a space child (redacted)");
        },
        AnyOtherFullStateEventContent::SpaceParent(FullStateEventContent::Redacted(_)) => {
            return Cow::Borrowed("* added a parent space (redacted)");
        },

        // Handle unknown events:
        e => {
            format!("* sent an unknown state event: {:?}", e.event_type())
        },
    };

    Cow::Owned(event)
}

pub fn html_membership(change: &RoomMembershipChange) -> StyleTree {
    let user_id = StyleTreeNode::UserId(change.user_id().to_owned());

    let children = match change.change() {
        None => {
            let prefix = StyleTreeNode::Text("* changed ".into());
            let suffix = StyleTreeNode::Text(" in unknown ways".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::None) => {
            let prefix = StyleTreeNode::Text("* did nothing to ".into());
            vec![prefix, user_id]
        },
        Some(MembershipChange::Error) => {
            let prefix = StyleTreeNode::Text("* failed to calculate membership change to ".into());
            vec![prefix, user_id]
        },
        Some(MembershipChange::NotImplemented) => {
            let prefix = StyleTreeNode::Text("* changed ".into());
            let suffix = StyleTreeNode::Text(" in unknown ways".into());
            vec![prefix, user_id, suffix]
        },

        Some(MembershipChange::Joined) => {
            vec![StyleTreeNode::Text("* joined the room".into())]
        },
        Some(MembershipChange::Left) => {
            vec![StyleTreeNode::Text("* left the room".into())]
        },
        Some(MembershipChange::Banned) => {
            let prefix = StyleTreeNode::Text("* banned ".into());
            let suffix = StyleTreeNode::Text(" from the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::Unbanned) => {
            let prefix = StyleTreeNode::Text("* unbanned ".into());
            let suffix = StyleTreeNode::Text(" from the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::Kicked) => {
            let prefix = StyleTreeNode::Text("* kicked ".into());
            let suffix = StyleTreeNode::Text(" from the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::Invited) => {
            let prefix = StyleTreeNode::Text("* invited ".into());
            let suffix = StyleTreeNode::Text(" to the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::KickedAndBanned) => {
            let prefix = StyleTreeNode::Text("* kicked and banned ".into());
            let suffix = StyleTreeNode::Text(" from the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::InvitationAccepted) => {
            vec![StyleTreeNode::Text(
                "* accepted an invitation to join the room".into(),
            )]
        },
        Some(MembershipChange::InvitationRejected) => {
            vec![StyleTreeNode::Text(
                "* rejected an invitation to join the room".into(),
            )]
        },
        Some(MembershipChange::InvitationRevoked) => {
            let prefix = StyleTreeNode::Text("* revoked an invitation for ".into());
            let suffix = StyleTreeNode::Text(" to join the room".into());
            vec![prefix, user_id, suffix]
        },
        Some(MembershipChange::Knocked) => {
            vec![StyleTreeNode::Text("* would like to join the room".into())]
        },
        Some(MembershipChange::KnockAccepted) => {
            let prefix = StyleTreeNode::Text("* accepted the room knock from ".into());
            vec![prefix, user_id]
        },
        Some(MembershipChange::KnockRetracted) => {
            vec![StyleTreeNode::Text("* retracted their room knock".into())]
        },
        Some(MembershipChange::KnockDenied) => {
            let prefix = StyleTreeNode::Text("* rejected the room knock from ".into());
            vec![prefix, user_id]
        },
    };

    StyleTree { children }
}

pub fn html_profile(change: &MemberProfileChange) -> StyleTree {
    let user_id = change.user_id().to_owned();

    let children = match (change.displayname_change(), change.avatar_url_change()) {
        (Some(change), avatar_change) => {
            let mut m = match (&change.old, &change.new) {
                (None, Some(new)) => {
                    vec![
                        StyleTreeNode::Text("* set their display name to ".into()),
                        StyleTreeNode::DisplayName(new.into(), user_id),
                    ]
                },
                (Some(old), Some(new)) => {
                    vec![
                        StyleTreeNode::Text("* changed their display name from ".into()),
                        StyleTreeNode::DisplayName(old.into(), user_id.clone()),
                        StyleTreeNode::Text(" to ".into()),
                        StyleTreeNode::DisplayName(new.into(), user_id),
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
                m.push(StyleTreeNode::Text(" and changed their user avatar".into()));
            }

            m
        },
        (None, Some(change)) => {
            let m = match (&change.old, &change.new) {
                (None, Some(_)) => Cow::Borrowed("* added a user avatar"),
                (Some(_), Some(_)) => Cow::Borrowed("* changed their user avatar"),
                (Some(_), None) => Cow::Borrowed("* removed their user avatar"),
                (None, None) => Cow::Borrowed("* made an unknown change to their user avatar"),
            };

            vec![StyleTreeNode::Text(m)]
        },
        (None, None) => {
            vec![StyleTreeNode::Text("* changed their user profile".into())]
        },
    };
    StyleTree { children }
}

pub fn html_state(change: &OtherState) -> StyleTree {
    let children = match change.content() {
        AnyOtherFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::PolicyRuleServer(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::PolicyRuleUser(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::RoomAliases(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* set the room aliases to: ".into());
            let mut cs = vec![prefix];

            for (i, alias) in content.aliases.iter().enumerate() {
                if i != 0 {
                    cs.push(StyleTreeNode::Text(", ".into()));
                }

                cs.push(StyleTreeNode::RoomAlias(alias.clone()));
            }

            cs
        },
        AnyOtherFullStateEventContent::RoomAvatar(FullStateEventContent::Original {
            content,
            prev_content,
        }) => {
            let prev_url = prev_content.as_ref().and_then(|p| p.url.as_ref());

            let node = match (prev_url, &content.url) {
                (None, Some(_)) => StyleTreeNode::Text("* added a room avatar".into()),
                (Some(old), Some(new)) => {
                    if old != new {
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
        AnyOtherFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Original {
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
        AnyOtherFullStateEventContent::RoomCreate(FullStateEventContent::Original {
            content,
            ..
        }) => {
            if content.federate {
                vec![StyleTreeNode::Text("* created a federated room".into())]
            } else {
                vec![StyleTreeNode::Text("* created a non-federated room".into())]
            }
        },
        AnyOtherFullStateEventContent::RoomEncryption(FullStateEventContent::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomGuestAccess(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let access = bold(format!("{:?}", content.guest_access.as_str()));
            let prefix = StyleTreeNode::Text("* set guest access for the room to ".into());
            vec![prefix, access]
        },
        AnyOtherFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix =
                StyleTreeNode::Text("* updated history visibility for the room to ".into());
            let vis = bold(format!("{:?}", content.history_visibility.as_str()));
            vec![prefix, vis]
        },
        AnyOtherFullStateEventContent::RoomJoinRules(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* update the join rules for the room to ".into());
            let rule = bold(format!("{:?}", content.join_rule.as_str()));
            vec![prefix, rule]
        },
        AnyOtherFullStateEventContent::RoomName(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* updated the room name to ".into());
            let name = bold(format!("{:?}", content.name));
            vec![prefix, name]
        },
        AnyOtherFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomPowerLevels(FullStateEventContent::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomServerAcl(FullStateEventContent::Original {
            ..
        }) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* sent a third-party invite to ".into());
            let name = bold(format!("{:?}", content.display_name));
            vec![prefix, name]
        },
        AnyOtherFullStateEventContent::RoomTombstone(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* upgraded the room; replacement room is ".into());
            let room = StyleTreeNode::RoomId(content.replacement_room.clone());
            vec![prefix, room]
        },
        AnyOtherFullStateEventContent::RoomTopic(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = StyleTreeNode::Text("* set the room topic to ".into());
            let topic = bold(format!("{:?}", content.topic));
            vec![prefix, topic]
        },
        AnyOtherFullStateEventContent::SpaceChild(FullStateEventContent::Original { .. }) => {
            let prefix = StyleTreeNode::Text("* added a space child: ".into());

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(change.state_key()) {
                StyleTreeNode::RoomId(room_id)
            } else {
                bold(change.state_key().to_string())
            };

            vec![prefix, room_id]
        },
        AnyOtherFullStateEventContent::SpaceParent(FullStateEventContent::Original {
            content,
            ..
        }) => {
            let prefix = if content.canonical {
                StyleTreeNode::Text("* added a canonical parent space: ".into())
            } else {
                StyleTreeNode::Text("* added a parent space: ".into())
            };

            let room_id = if let Ok(room_id) = OwnedRoomId::from_str(change.state_key()) {
                StyleTreeNode::RoomId(room_id)
            } else {
                bold(change.state_key().to_string())
            };

            vec![prefix, room_id]
        },

        // Redacted variants of state events:
        AnyOtherFullStateEventContent::PolicyRuleRoom(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a room policy rule (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::PolicyRuleServer(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a server policy rule (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::PolicyRuleUser(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated a user policy rule (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomAliases(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room aliases for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomAvatar(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room avatar (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomCanonicalAlias(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the canonical alias for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomCreate(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text("* created the room (redacted)".into())]
        },
        AnyOtherFullStateEventContent::RoomEncryption(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the encryption settings for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomGuestAccess(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the guest access configuration for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomHistoryVisibility(FullStateEventContent::Redacted(
            _,
        )) => {
            vec![StyleTreeNode::Text(
                "* updated history visilibity for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomJoinRules(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the join rules for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomName(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room name (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomPinnedEvents(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the pinned events for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomPowerLevels(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the power levels for the room (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomServerAcl(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room's server ACLs (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomThirdPartyInvite(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* sent a third-party invite (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::RoomTombstone(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text("* upgraded the room (redacted)".into())]
        },
        AnyOtherFullStateEventContent::RoomTopic(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* updated the room topic (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::SpaceChild(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a space child (redacted)".into(),
            )]
        },
        AnyOtherFullStateEventContent::SpaceParent(FullStateEventContent::Redacted(_)) => {
            vec![StyleTreeNode::Text(
                "* added a parent space (redacted)".into(),
            )]
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
