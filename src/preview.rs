use std::{collections::HashMap, sync::Arc};

use matrix_sdk::{
    media::{MediaFormat, MediaRequestParameters, UniqueKey},
    ruma::{
        events::{
            room::{
                message::{MessageType, RoomMessageEventContent},
                MediaSource,
            },
            MessageLikeEvent,
        },
        OwnedEventId,
    },
    Media,
};
use ratatui::layout::Rect;
use ratatui_image::{picker::Picker, protocol::Protocol, Resize};
use tokio::sync::Semaphore;

use crate::{
    base::{AsyncProgramStore, IambError},
    config::{ApplicationSettings, ImagePreviewSize},
    worker::Requester,
};

pub enum ImageStatus {
    Queued(ImagePreviewSize),
    Downloading(ImagePreviewSize),
    Loaded(Protocol),
    Error(String),
}

pub struct PreviewManager {
    /// Image preview "protocol" picker.
    picker: Option<Arc<Picker>>,

    /// Permits for rendering images in background thread.
    permits: Arc<Semaphore>,

    /// Indexed by [`MediaSource::unique_key`]
    previews: HashMap<String, ImageStatus>,
}

impl PreviewManager {
    pub fn new(picker: Option<Picker>) -> Self {
        Self {
            picker: picker.map(Into::into),
            permits: Arc::new(Semaphore::new(2)),
            previews: Default::default(),
        }
    }

    pub fn get(&self, source: &MediaSource) -> Option<&ImageStatus> {
        self.previews.get(&source.unique_key())
    }

    fn insert(&mut self, key: String, status: ImageStatus) {
        self.previews.insert(key, status);
    }

    /// Queue download and preparation of preview
    pub fn load(&mut self, source: &MediaSource, worker: &Requester) {
        let Some(status) = self.previews.get_mut(&source.unique_key()) else {
            return;
        };
        let Some(picker) = &self.picker else { return };

        if let ImageStatus::Queued(size) = status {
            let size = *size;
            *status = ImageStatus::Downloading(size);

            worker.load_image(
                source.to_owned(),
                size.to_owned(),
                Arc::clone(picker),
                Arc::clone(&self.permits),
            );
        }
    }

    pub fn register_preview(
        &mut self,
        settings: &ApplicationSettings,
        source: MediaSource,
        size: ImagePreviewSize,
        worker: &Requester,
    ) {
        if self.picker.is_none() {
            return;
        }

        let key = source.unique_key();
        if self.previews.contains_key(&key) {
            return;
        }
        self.previews.insert(key, ImageStatus::Queued(size));

        if settings
            .tunables
            .image_preview
            .as_ref()
            .is_some_and(|setting| !setting.lazy_load)
        {
            self.load(&source, worker);
        }
    }
}

pub fn source_from_event(
    ev: &MessageLikeEvent<RoomMessageEventContent>,
) -> Option<(OwnedEventId, MediaSource)> {
    if let MessageLikeEvent::Original(ev) = &ev {
        if let MessageType::Image(c) = &ev.content.msgtype {
            return Some((ev.event_id.clone(), c.source.clone()));
        }
    }
    None
}

impl From<ImagePreviewSize> for Rect {
    fn from(value: ImagePreviewSize) -> Self {
        Rect::new(0, 0, value.width as _, value.height as _)
    }
}
impl From<Rect> for ImagePreviewSize {
    fn from(rect: Rect) -> Self {
        ImagePreviewSize { width: rect.width as _, height: rect.height as _ }
    }
}

pub async fn load_image(
    store: AsyncProgramStore,
    media: Media,
    source: MediaSource,
    picker: Arc<Picker>,
    permits: Arc<Semaphore>,
    size: ImagePreviewSize,
) {
    async fn load_image_inner(
        media: Media,
        source: MediaSource,
        picker: Arc<Picker>,
        permits: Arc<Semaphore>,
        size: ImagePreviewSize,
    ) -> Result<ImageStatus, IambError> {
        let reader = media
            .get_media_content(&MediaRequestParameters { source, format: MediaFormat::File }, true)
            .await
            .map(std::io::Cursor::new)
            .map(image::ImageReader::new)
            .map_err(IambError::Matrix)
            .and_then(|reader| reader.with_guessed_format().map_err(IambError::IOError))?;

        let image = reader.decode().map_err(IambError::Image)?;

        let permit = permits
            .acquire()
            .await
            .map_err(|err| IambError::Preview(err.to_string()))?;

        let handle = tokio::task::spawn_blocking(move || {
            picker
                .new_protocol(image, size.into(), Resize::Fit(None))
                .map_err(|err| IambError::Preview(err.to_string()))
        });

        let image = handle.await.map_err(|err| IambError::Preview(err.to_string()))??;
        std::mem::drop(permit);

        Ok(ImageStatus::Loaded(image))
    }
    let key = source.unique_key();

    let status = match load_image_inner(media, source, picker, permits, size).await {
        Ok(status) => status,
        Err(err) => ImageStatus::Error(format!("{err:?}")),
    };

    let mut locked = store.lock().await;
    locked.application.previews.insert(key, status);
}
