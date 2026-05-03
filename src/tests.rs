use std::borrow::Cow;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use std::{collections::HashMap, sync::Weak};

use chrono::DateTime;
use matrix_sdk::ruma::events::receipt::Receipt;
use matrix_sdk::ruma::events::room::MediaSource;
use matrix_sdk::ruma::{
    event_id,
    events::room::message::RoomMessageEventContent,
    server_name,
    user_id,
    EventId,
    OwnedEventId,
    OwnedRoomId,
    OwnedUserId,
    RoomId,
};
use matrix_sdk::ruma::{MilliSecondsSinceUnixEpoch, UserId};

use lazy_static::lazy_static;
use matrix_sdk_ui::eyeball_im::Vector;
use matrix_sdk_ui::timeline::{
    EventTimelineItem,
    MsgLikeContent,
    Profile,
    TimelineDetails,
    TimelineEventItemId,
    TimelineItem,
    TimelineItemContent,
    TimelineUniqueId,
    VirtualTimelineItem,
};
use modalkit::prelude::ViewportContext;
use ratatui::style::{Color, Modifier as StyleModifier, Style};
use ratatui::text::{Line, Span, Text};
use tokio::sync::mpsc::unbounded_channel;
use url::Url;

use crate::message::{
    MessageCursor,
    MessageExt,
    MessageTimeStamp,
    ProtocolPreview,
    TimelineItemExt,
};
use crate::preview::PreviewManager;
use crate::util::space_span;
use crate::{
    base::{ChatStore, ProgramStore, RoomInfo},
    config::{
        user_color,
        user_style_from_color,
        ApplicationSettings,
        DirectoryValues,
        Notifications,
        NotifyVia,
        ProfileConfig,
        SortOverrides,
        TunableValues,
        UserColor,
        UserDisplayStyle,
        UserDisplayTunables,
    },
    message::{Message, MessageKey, Messages},
    worker::Requester,
};

const TEST_ROOM1_ALIAS: &str = "#room1:example.com";

lazy_static! {
    pub static ref TEST_ROOM1_ID: OwnedRoomId =
        RoomId::new_v1(server_name!("example.com")).to_owned();
    pub static ref TEST_USER1: OwnedUserId = user_id!("@user1:example.com").to_owned();
    pub static ref TEST_USER2: OwnedUserId = user_id!("@user2:example.com").to_owned();
    pub static ref TEST_USER3: OwnedUserId = user_id!("@user3:example.com").to_owned();
    pub static ref TEST_USER4: OwnedUserId = user_id!("@user4:example.com").to_owned();
    pub static ref TEST_USER5: OwnedUserId = user_id!("@user5:example.com").to_owned();
    pub static ref MSG1_EVID: TimelineEventItemId =
        TimelineEventItemId::EventId(EventId::new(server_name!("example.com")));
    pub static ref MSG2_EVID: TimelineEventItemId =
        TimelineEventItemId::EventId(EventId::new(server_name!("example.com")));
    pub static ref MSG3_EVID: TimelineEventItemId =
        TimelineEventItemId::EventId(event_id!("$5jRz3KfVhaUzXtVj7k:example.com").to_owned());
    pub static ref MSG4_EVID: TimelineEventItemId =
        TimelineEventItemId::EventId(event_id!("$JP6qFV7WyXk5ZnexM3:example.com").to_owned());
    pub static ref MSG5_EVID: TimelineEventItemId =
        TimelineEventItemId::EventId(EventId::new(server_name!("example.com")));
    pub static ref MSG1_KEY: MessageKey =
        MessageKey::new(6, TimelineUniqueId("MSG1_KEY".to_string()));
    pub static ref MSG2_KEY: MessageKey =
        MessageKey::new(1, TimelineUniqueId("MSG2_KEY".to_string()));
    pub static ref MSG3_KEY: MessageKey =
        MessageKey::new(2, TimelineUniqueId("MSG3_KEY".to_string()));
    pub static ref MSG4_KEY: MessageKey =
        MessageKey::new(3, TimelineUniqueId("MSG4_KEY".to_string()));
    pub static ref MSG5_KEY: MessageKey =
        MessageKey::new(4, TimelineUniqueId("MSG5_KEY".to_string()));
    pub static ref DIVIDER1_KEY: MessageKey =
        MessageKey::new(0, TimelineUniqueId("_divider1".to_string()));
    pub static ref DIVIDER2_KEY: MessageKey =
        MessageKey::new(5, TimelineUniqueId("_divider2".to_string()));
}

#[derive(Debug)]
pub struct MockMessage {
    text: &'static str,
    content: TimelineItemContent,
    sender: OwnedUserId,
    id: TimelineEventItemId,
}

impl Deref for MockMessage {
    type Target = EventTimelineItem;

    fn deref(&self) -> &Self::Target {
        unimplemented!()
    }
}

impl MessageExt for MockMessage {
    fn content(&self) -> &TimelineItemContent {
        &self.content
    }

    fn sender(&self) -> &UserId {
        &self.sender
    }

    fn sender_profile(&self) -> &TimelineDetails<Profile> {
        &TimelineDetails::Pending
    }

    fn message_timestamp(&self) -> MessageTimeStamp {
        unimplemented!()
    }

    fn identifier(&self) -> TimelineEventItemId {
        self.id.clone()
    }

    fn is_local_echo(&self) -> bool {
        matches!(self.identifier(), TimelineEventItemId::TransactionId(_))
    }

    fn read_receipts(&self) -> impl Iterator<Item = (&OwnedUserId, &Receipt)> {
        vec![].into_iter()
    }

    fn is_edited(&self) -> bool {
        false
    }

    // --- overrides ---

    fn body(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.text)
    }
    fn image_preview(&self) -> Option<&MediaSource> {
        None
    }
}

impl MockMessage {
    pub fn event_id(&self) -> Option<&EventId> {
        None
    }
}

#[derive(Debug)]
pub struct MockMessageItem {
    id: TimelineUniqueId,
    /// Either a message or a date divider
    inner: Option<MockMessage>,
}

impl Deref for MockMessageItem {
    type Target = TimelineItem;

    fn deref(&self) -> &Self::Target {
        unimplemented!()
    }
}

impl TimelineItemExt for MockMessageItem {
    fn item(&self) -> &TimelineItem {
        unimplemented!()
    }

    fn sender(&self) -> Option<&UserId> {
        self.inner.as_ref().map(|inner| inner.sender())
    }

    fn show_with_preview<'a>(
        &'a self,
        prev_sender: Option<&UserId>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
    ) -> (Text<'a>, [Option<ProtocolPreview<'a>>; 2]) {
        match &self.inner {
            Some(inner) => {
                inner.show_with_preview(prev_sender, selected, vwctx, info, settings, previews)
            },
            None => {
                let time = DateTime::from_timestamp_nanos(0).format("%A, %B %d %Y").to_string();
                let padding = vwctx.get_width().saturating_sub(time.len());
                let leading = space_span(padding / 2, Style::default());
                let trailing = space_span(padding.saturating_sub(padding / 2), Style::default());
                let time = Span::styled(time, Style::new().add_modifier(StyleModifier::BOLD));

                (Line::from(vec![leading, time, trailing]).into(), [None, None])
            },
        }
    }
}

impl MockMessageItem {
    fn new_message(
        content: &'static str,
        sender: OwnedUserId,
        key: &MessageKey,
        id: TimelineEventItemId,
    ) -> Self {
        Self {
            id: key.id().to_owned(),
            inner: Some(MockMessage {
                id,
                sender,
                text: content,
                content: TimelineItemContent::MsgLike(MsgLikeContent::redacted()),
            }),
        }
    }

    pub fn unique_id(&self) -> &TimelineUniqueId {
        &self.id
    }

    pub fn as_event(&self) -> Option<&Message> {
        self.inner.as_ref()
    }

    pub fn as_virtual(&self) -> Option<VirtualTimelineItem> {
        if self.inner.is_none() {
            Some(VirtualTimelineItem::DateDivider(MilliSecondsSinceUnixEpoch(0u16.into())))
        } else {
            None
        }
    }

    pub fn is_timeline_start(&self) -> bool {
        false
    }
}

pub fn user_style(user: &str) -> Style {
    user_style_from_color(user_color(user))
}

pub fn mock_message1() -> MockMessageItem {
    let content = "writhe";

    MockMessageItem::new_message(content, TEST_USER1.clone(), &MSG1_KEY, MSG1_EVID.clone())
}

pub fn mock_message2() -> MockMessageItem {
    let content = "helium";

    MockMessageItem::new_message(content, TEST_USER2.clone(), &MSG2_KEY, MSG2_EVID.clone())
}

pub fn mock_message3() -> MockMessageItem {
    let content = "this\nis\na\nmultiline\nmessage";

    MockMessageItem::new_message(content, TEST_USER2.clone(), &MSG3_KEY, MSG3_EVID.clone())
}

pub fn mock_message4() -> MockMessageItem {
    let content = "help";

    MockMessageItem::new_message(content, TEST_USER1.clone(), &MSG4_KEY, MSG4_EVID.clone())
}

pub fn mock_message5() -> MockMessageItem {
    let content = "character";

    MockMessageItem::new_message(content, TEST_USER2.clone(), &MSG5_KEY, MSG5_EVID.clone())
}

pub fn mock_messages() -> Vector<Arc<MockMessageItem>> {
    [
        MockMessageItem { id: DIVIDER1_KEY.id().to_owned(), inner: None }.into(),
        mock_message2().into(),
        mock_message3().into(),
        mock_message4().into(),
        mock_message5().into(),
        MockMessageItem { id: DIVIDER2_KEY.id().to_owned(), inner: None }.into(),
        mock_message1().into(),
    ]
    .into()
}

pub fn mock_dirs() -> DirectoryValues {
    DirectoryValues {
        cache: PathBuf::new(),
        data: PathBuf::new(),
        logs: PathBuf::new(),
        downloads: None,
    }
}

pub fn mock_tunables() -> TunableValues {
    TunableValues {
        default_room: None,
        log_level: "warn".into(),
        max_log_files: 7,
        message_shortcode_display: false,
        normal_after_send: true,
        reaction_display: true,
        reaction_shortcode_display: false,
        read_receipt_send: true,
        read_receipt_display: true,
        request_timeout: 120,
        sort: SortOverrides::default().values(),
        state_event_display: true,
        typing_notice_send: true,
        typing_notice_display: true,
        users: vec![(TEST_USER5.clone(), UserDisplayTunables {
            color: Some(UserColor(Color::Black)),
            name: Some("USER 5".into()),
        })]
        .into_iter()
        .collect::<HashMap<_, _>>(),
        open_command: None,
        external_edit_file_suffix: String::from(".md"),
        username_display: UserDisplayStyle::Username,
        message_user_color: false,
        mouse: Default::default(),
        notifications: Notifications {
            enabled: false,
            via: NotifyVia::default(),
            show_message: true,
            sound_hint: None,
        },
        image_preview: None,
        user_gutter_width: 30,
        tabstop: 4,
    }
}

pub fn mock_settings() -> ApplicationSettings {
    ApplicationSettings {
        layout_json: PathBuf::new(),
        session_json: PathBuf::new(),
        session_json_old: PathBuf::new(),
        sled_dir: PathBuf::new(),
        sqlite_dir: PathBuf::new(),

        profile_name: "test".into(),
        profile: ProfileConfig {
            user_id: user_id!("@user:example.com").to_owned(),
            url: None,
            settings: None,
            dirs: None,
            layout: None,
            macros: None,
        },
        tunables: mock_tunables(),
        dirs: mock_dirs(),
        layout: Default::default(),
        macros: HashMap::default(),
    }
}

pub async fn mock_store() -> ProgramStore {
    let (tx, _) = unbounded_channel();
    let homeserver = Url::parse("https://localhost").unwrap();
    let client = matrix_sdk::Client::new(homeserver).await.unwrap();
    let worker = Requester { tx, client, store: Weak::new() };

    let mut store = ChatStore::new(worker, mock_settings());

    // Add presence information.
    store.presences.get_or_default(TEST_USER1.clone());
    store.presences.get_or_default(TEST_USER2.clone());
    store.presences.get_or_default(TEST_USER3.clone());
    store.presences.get_or_default(TEST_USER4.clone());
    store.presences.get_or_default(TEST_USER5.clone());

    let room_id = TEST_ROOM1_ID.clone();
    let mut info = RoomInfo::mock_new();

    info.get_thread_mut(None).unwrap().set_messages(mock_messages());

    store.rooms.insert(room_id.clone(), info);
    store.names.insert(TEST_ROOM1_ALIAS.to_string(), room_id);
    ProgramStore::new(store)
}
