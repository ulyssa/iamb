use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

use indexmap::IndexMap;
use matrix_sdk::ruma::events::poll::end::{OriginalPollEndEvent, PollEndEventContent};
use matrix_sdk::ruma::events::poll::response::OriginalPollResponseEvent;
use matrix_sdk::ruma::events::poll::start::{
    PollContentBlock,
    PollKind,
    PollStartEventContent,
    PollStartEventContentWithoutRelation,
};
use matrix_sdk::ruma::events::poll::unstable_end::{
    OriginalUnstablePollEndEvent,
    UnstablePollEndEventContent,
};
use matrix_sdk::ruma::events::poll::unstable_response::OriginalUnstablePollResponseEvent;
use matrix_sdk::ruma::events::poll::unstable_start::{
    NewUnstablePollStartEventContent,
    ReplacementUnstablePollStartEventContent,
    UnstablePollStartContentBlock,
};
use matrix_sdk::ruma::events::relation::Thread;
use matrix_sdk::ruma::events::room::message::{Relation, RelationWithoutReplacement};
use matrix_sdk::ruma::events::{OriginalMessageLikeEvent, poll};
use matrix_sdk::ruma::{EventId, MilliSecondsSinceUnixEpoch, OwnedEventId, OwnedUserId, UserId};

use crate::message::{MessageKey, MessageTimeStamp};

/// Indicates where an [EventId] lives in the poll.
#[derive(Clone, Debug)]
pub enum PollEventLocation {
    /// The [EventId] belongs to a replacement and has key [MessageKey].
    Replacement(MessageKey),
    /// The [EventId] belongs to a response and has key [MessageKey].
    Response(MessageKey),
    /// The [EventId] belongs to the end event.
    End,
}

/// The first entry in the tuples produced by the iterators is the answer id.
fn body_cow<'a>(
    kind: &'a PollKind,
    own_user_id: &'a UserId,
    question: &'a str,
    answers: impl IntoIterator<Item = (&'a str, &'a str)>,
    results: impl IntoIterator<Item = (&'a str, BTreeSet<&'a UserId>)>,
    ended: Option<MilliSecondsSinceUnixEpoch>,
) -> String {
    // map answer id to answer text and the char `'X'` if the user has selected this option or `' '`
    // otherwise
    let mut answers: IndexMap<_, _> = answers.into_iter().map(|(k, v)| (k, (v, ' '))).collect();

    let mut text = "Poll: ".to_string();
    text.push_str(question);

    let show_user_count = ended.is_some() || *kind == PollKind::Disclosed;
    for (id, users) in results {
        let (answer_text, selection_char) = answers.get_mut(id).unwrap();

        if users.contains(own_user_id) {
            *selection_char = 'X';
        }

        if show_user_count {
            write!(&mut text, "\n[{selection_char}] ({}) {answer_text}", users.len())
                .expect("writing to a String cannot return an error");
        }
    }

    if !show_user_count {
        for (answer_text, selection_char) in answers.values() {
            write!(&mut text, "\n[{selection_char}] {answer_text}")
                .expect("writing to a String cannot return an error");
        }
    }

    if let Some(ended_ts) = ended {
        let ended_time = MessageTimeStamp(ended_ts).as_datetime();

        write!(&mut text, "\nEnded: {}", ended_time.format("%c"))
            .expect("writing to a String cannot return an error");
    }

    text
}

/// All data related to a poll in an unsupported room version.
#[derive(Debug, Clone)]
pub struct Poll {
    event_id: OwnedEventId,
    own_user_id: OwnedUserId,
    pub start: PollStartEventContent,
    pub replacements: BTreeMap<MessageKey, PollStartEventContentWithoutRelation>,

    pub responeses: BTreeMap<MessageKey, OriginalPollResponseEvent>,

    pub end: Option<OriginalMessageLikeEvent<PollEndEventContent>>,
}

impl Poll {
    pub fn new(
        event_id: OwnedEventId,
        own_user_id: OwnedUserId,
        start: PollStartEventContent,
        unloaded: UnloadedPoll,
    ) -> Self {
        Self {
            event_id,
            own_user_id,
            start,
            replacements: unloaded.replacements,
            responeses: unloaded.responeses,
            end: unloaded.end,
        }
    }

    fn content(&self) -> &PollContentBlock {
        self.replacements
            .iter()
            .rev()
            .map(|(_, v)| &v.poll)
            .next()
            .unwrap_or(&self.start.poll)
    }

    // fn results(&self) -> IndexMap<&str, BTreeSet<&UserId>> {
    fn results(&self) -> impl IntoIterator<Item = (&str, BTreeSet<&UserId>)> {
        let poll = self.content();
        let responses = self.responeses.values().map(OriginalPollResponseEvent::data);
        let end_timestamp = self.end.as_ref().map(|end| end.origin_server_ts);

        poll::compile_poll_results(poll, responses, end_timestamp)
    }

    pub fn event_id(&self) -> &EventId {
        self.event_id.as_ref()
    }

    /// The message this poll replied to if it is a reply.
    pub fn reply_to(&self) -> Option<&EventId> {
        match &self.start.relates_to {
            Some(Relation::Reply(reply)) => Some(reply.in_reply_to.event_id.as_ref()),
            Some(Relation::Thread(Thread {
                in_reply_to: Some(in_reply_to),
                is_falling_back: false,
                ..
            })) => Some(in_reply_to.event_id.as_ref()),
            _ => None,
        }
    }

    /// Return the thread root if this is the first message in the thread.
    pub fn thread_root(&self) -> Option<&EventId> {
        match &self.start.relates_to {
            Some(Relation::Thread(Thread {
                event_id,
                in_reply_to: Some(in_reply_to),
                is_falling_back: true,
                ..
            })) if event_id == &in_reply_to.event_id => Some(event_id.as_ref()),
            _ => None,
        }
    }

    pub fn redact(&mut self, loc: &PollEventLocation) {
        match loc {
            PollEventLocation::Replacement(key) => {
                self.replacements.remove(key);
            },
            PollEventLocation::Response(key) => {
                self.responeses.remove(key);
            },
            PollEventLocation::End => self.end = None,
        }
    }

    pub fn insert_relation(&mut self, value: PollRelation) -> PollEventLocation {
        match value {
            PollRelation::Response(ev) => {
                let key = MessageKey {
                    ts: ev.origin_server_ts.into(),
                    id: ev.event_id.clone().into(),
                };

                self.responeses.insert(key.clone(), ev);

                PollEventLocation::Response(key)
            },
            PollRelation::End(ev) => {
                self.end = Some(ev);
                PollEventLocation::End
            },
        }
    }

    pub fn body_cow(&self) -> Cow<'_, str> {
        let start = self.content();

        let question = start.question.text.find_plain().unwrap_or("Question not found");
        let answers = start
            .answers
            .iter()
            .filter_map(|answer| answer.text.find_plain().map(|text| (answer.id.as_str(), text)));
        let results = self.results();
        let ended = self.end.as_ref().map(|ev| ev.origin_server_ts);

        body_cow(&start.kind, &self.own_user_id, question, answers, results, ended).into()
    }
}

/// All data accumulated for a [`Poll`] before the start event was loaded.
#[derive(Debug, Clone, Default)]
pub struct UnloadedPoll {
    pub replacements: BTreeMap<MessageKey, PollStartEventContentWithoutRelation>,

    pub responeses: BTreeMap<MessageKey, OriginalPollResponseEvent>,

    pub end: Option<OriginalMessageLikeEvent<PollEndEventContent>>,
}

impl UnloadedPoll {
    pub fn redact(&mut self, loc: &PollEventLocation) {
        match loc {
            PollEventLocation::Replacement(key) => {
                self.replacements.remove(key);
            },
            PollEventLocation::Response(key) => {
                self.responeses.remove(key);
            },
            PollEventLocation::End => self.end = None,
        }
    }

    pub fn insert_relation(&mut self, value: PollRelation) -> PollEventLocation {
        match value {
            PollRelation::Response(ev) => {
                let key = MessageKey {
                    ts: ev.origin_server_ts.into(),
                    id: ev.event_id.clone().into(),
                };

                self.responeses.insert(key.clone(), ev);

                PollEventLocation::Response(key)
            },
            PollRelation::End(ev) => {
                self.end = Some(ev);
                PollEventLocation::End
            },
        }
    }
}

/// A response or end event that is stored with the poll. Used for more unified
/// insertion logic.
pub enum PollRelation {
    Response(OriginalPollResponseEvent),
    End(OriginalPollEndEvent),
}

impl PollRelation {
    pub fn event_id(&self) -> &EventId {
        match self {
            PollRelation::Response(ev) => &ev.event_id,
            PollRelation::End(ev) => &ev.event_id,
        }
    }

    /// The [`EventId`] of the poll start this event relates to.
    pub fn poll_event_id(&self) -> &EventId {
        match self {
            PollRelation::Response(ev) => &ev.content.relates_to.event_id,
            PollRelation::End(ev) => &ev.content.relates_to.event_id,
        }
    }
}

impl From<OriginalPollResponseEvent> for PollRelation {
    fn from(value: OriginalPollResponseEvent) -> Self {
        Self::Response(value)
    }
}
impl From<OriginalPollEndEvent> for PollRelation {
    fn from(value: OriginalPollEndEvent) -> Self {
        Self::End(value)
    }
}

/// All data related to a poll in an unsupported room version.
#[derive(Debug, Clone)]
pub struct UnstablePoll {
    event_id: OwnedEventId,
    own_user_id: OwnedUserId,
    pub start: NewUnstablePollStartEventContent,
    pub replacements: BTreeMap<MessageKey, ReplacementUnstablePollStartEventContent>,

    pub responeses: BTreeMap<MessageKey, OriginalUnstablePollResponseEvent>,

    pub end: Option<OriginalMessageLikeEvent<UnstablePollEndEventContent>>,
}

impl UnstablePoll {
    pub fn new(
        event_id: OwnedEventId,
        own_user_id: OwnedUserId,
        start: NewUnstablePollStartEventContent,
        unloaded: UnloadedUnstablePoll,
    ) -> Self {
        Self {
            event_id,
            own_user_id,
            start,
            replacements: unloaded.replacements,
            responeses: unloaded.responeses,
            end: unloaded.end,
        }
    }

    fn content(&self) -> &UnstablePollStartContentBlock {
        self.replacements
            .iter()
            .rev()
            .map(|(_, v)| &v.relates_to.new_content.poll_start)
            .next()
            .unwrap_or(&self.start.poll_start)
    }

    // fn results(&self) -> IndexMap<&str, BTreeSet<&UserId>> {
    fn results(&self) -> impl IntoIterator<Item = (&str, BTreeSet<&UserId>)> {
        let poll = self.content();
        let responses = self.responeses.values().map(OriginalUnstablePollResponseEvent::data);
        let end_timestamp = self.end.as_ref().map(|end| end.origin_server_ts);

        poll::compile_unstable_poll_results(poll, responses, end_timestamp)
    }

    pub fn event_id(&self) -> &EventId {
        self.event_id.as_ref()
    }

    /// The message this poll replied to if it is a reply.
    pub fn reply_to(&self) -> Option<&EventId> {
        match &self.start.relates_to {
            Some(RelationWithoutReplacement::Reply(reply)) => {
                Some(reply.in_reply_to.event_id.as_ref())
            },
            Some(RelationWithoutReplacement::Thread(Thread {
                in_reply_to: Some(in_reply_to),
                is_falling_back: false,
                ..
            })) => Some(in_reply_to.event_id.as_ref()),
            _ => None,
        }
    }

    /// Return the thread root if this is the first message in the thread.
    pub fn thread_root(&self) -> Option<&EventId> {
        match &self.start.relates_to {
            Some(RelationWithoutReplacement::Thread(Thread {
                event_id,
                in_reply_to: Some(in_reply_to),
                is_falling_back: true,
                ..
            })) if event_id == &in_reply_to.event_id => Some(event_id.as_ref()),
            _ => None,
        }
    }

    pub fn redact(&mut self, loc: &PollEventLocation) {
        match loc {
            PollEventLocation::Replacement(key) => {
                self.replacements.remove(key);
            },
            PollEventLocation::Response(key) => {
                self.responeses.remove(key);
            },
            PollEventLocation::End => self.end = None,
        }
    }

    pub fn insert_relation(&mut self, value: UnstablePollRelation) -> PollEventLocation {
        match value {
            UnstablePollRelation::Response(ev) => {
                let key = MessageKey {
                    ts: ev.origin_server_ts.into(),
                    id: ev.event_id.clone().into(),
                };

                self.responeses.insert(key.clone(), ev);

                PollEventLocation::Response(key)
            },
            UnstablePollRelation::End(ev) => {
                self.end = Some(ev);
                PollEventLocation::End
            },
        }
    }

    pub fn body_cow(&self) -> Cow<'_, str> {
        let start = self.content();

        let question = &start.question.text;
        let answers = start
            .answers
            .iter()
            .map(|answer| (answer.id.as_str(), answer.text.as_str()));
        let results = self.results();
        let ended = self.end.as_ref().map(|ev| ev.origin_server_ts);

        body_cow(&start.kind, &self.own_user_id, question, answers, results, ended).into()
    }
}

/// All data accumulated for a [`UnstablePoll`] before the start event was loaded.
#[derive(Debug, Clone, Default)]
pub struct UnloadedUnstablePoll {
    pub replacements: BTreeMap<MessageKey, ReplacementUnstablePollStartEventContent>,

    pub responeses: BTreeMap<MessageKey, OriginalUnstablePollResponseEvent>,

    pub end: Option<OriginalMessageLikeEvent<UnstablePollEndEventContent>>,
}

impl UnloadedUnstablePoll {
    pub fn redact(&mut self, loc: &PollEventLocation) {
        match loc {
            PollEventLocation::Replacement(key) => {
                self.replacements.remove(key);
            },
            PollEventLocation::Response(key) => {
                self.responeses.remove(key);
            },
            PollEventLocation::End => self.end = None,
        }
    }

    pub fn insert_relation(&mut self, value: UnstablePollRelation) -> PollEventLocation {
        match value {
            UnstablePollRelation::Response(ev) => {
                let key = MessageKey {
                    ts: ev.origin_server_ts.into(),
                    id: ev.event_id.clone().into(),
                };

                self.responeses.insert(key.clone(), ev);

                PollEventLocation::Response(key)
            },
            UnstablePollRelation::End(ev) => {
                self.end = Some(ev);
                PollEventLocation::End
            },
        }
    }
}

/// A response or end event that is stored with the poll. Used for more unified
/// insertion logic.
pub enum UnstablePollRelation {
    Response(OriginalUnstablePollResponseEvent),
    End(OriginalUnstablePollEndEvent),
}

impl UnstablePollRelation {
    pub fn event_id(&self) -> &EventId {
        match self {
            UnstablePollRelation::Response(ev) => &ev.event_id,
            UnstablePollRelation::End(ev) => &ev.event_id,
        }
    }

    /// The [`EventId`] of the poll start this event relates to.
    pub fn poll_event_id(&self) -> &EventId {
        match self {
            UnstablePollRelation::Response(ev) => &ev.content.relates_to.event_id,
            UnstablePollRelation::End(ev) => &ev.content.relates_to.event_id,
        }
    }
}

impl From<OriginalUnstablePollResponseEvent> for UnstablePollRelation {
    fn from(value: OriginalUnstablePollResponseEvent) -> Self {
        Self::Response(value)
    }
}
impl From<OriginalUnstablePollEndEvent> for UnstablePollRelation {
    fn from(value: OriginalUnstablePollEndEvent) -> Self {
        Self::End(value)
    }
}
