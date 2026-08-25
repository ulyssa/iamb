use std::{collections::HashMap, sync::Arc};

use matrix_sdk::{
    Media,
    media::{MediaFormat, MediaRequestParameters, UniqueKey},
    ruma::events::room::MediaSource,
};
use ratatui::layout::Rect;
use ratatui_image::{Resize, picker::Picker, protocol::Protocol};
use tokio::sync::Semaphore;

use crate::{
    base::{AsyncProgramStore, IambError},
    config::{ApplicationSettings, ImagePreviewSize, ImagePreviewValues},
    worker::Requester,
};

pub enum ImageStatus {
    Queued(ImagePreviewSize),
    Downloading(ImagePreviewSize),
    Loaded(Protocol),
    Error(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PreviewKind {
    Message,
    Reaction,
}

impl PreviewKind {
    fn image_size(self, image_preview: &ImagePreviewValues) -> ImagePreviewSize {
        match self {
            Self::Message => image_preview.size,
            Self::Reaction => ImagePreviewSize { width: 2, height: 1 },
        }
    }
}

pub struct PreviewManager {
    /// Image preview "protocol" picker.
    picker: Arc<Picker>,

    /// Permits for rendering images in background thread.
    permits: Arc<Semaphore>,

    /// Indexed by [`MediaSource::unique_key`]
    previews: HashMap<(String, PreviewKind), ImageStatus>,
}

impl PreviewManager {
    pub fn new(settings: &ApplicationSettings) -> Self {
        let picker = picker_from_settings(settings);

        Self {
            picker: picker.into(),
            permits: Arc::new(Semaphore::new(2)),
            previews: Default::default(),
        }
    }

    pub fn get(&self, source: &MediaSource, kind: PreviewKind) -> Option<&ImageStatus> {
        self.previews.get(&(source.unique_key(), kind))
    }

    fn insert(&mut self, key: String, kind: PreviewKind, status: ImageStatus) {
        self.previews.insert((key, kind), status);
    }

    /// Queue download and preparation of preview
    pub fn load(&mut self, source: &MediaSource, kind: PreviewKind, worker: &Requester) {
        let Some(status) = self.previews.get_mut(&(source.unique_key(), kind)) else {
            return;
        };

        if let ImageStatus::Queued(size) = status {
            let size = *size;
            *status = ImageStatus::Downloading(size);

            worker.load_image(
                source.to_owned(),
                kind,
                size.to_owned(),
                Arc::clone(&self.picker),
                Arc::clone(&self.permits),
            );
        }
    }

    pub fn register_preview(
        &mut self,
        settings: &ApplicationSettings,
        source: &MediaSource,
        kind: PreviewKind,
        worker: &Requester,
    ) {
        let key = (source.unique_key(), kind);
        if self.previews.contains_key(&key) {
            return;
        }

        let size = kind.image_size(&settings.tunables.image_preview);
        self.previews.insert(key, ImageStatus::Queued(size));

        if settings.tunables.image_preview.enabled && !settings.tunables.image_preview.lazy_load {
            self.load(source, kind, worker);
        }
    }
}

fn picker_from_settings(settings: &ApplicationSettings) -> Picker {
    // XXX: documentation says to use this query on alternate screen but it seems to be fine
    match Picker::from_query_stdio() {
        Ok(mut picker) => {
            // user forced protocol type; use that
            if let Some(protocol_type) = settings.tunables.image_preview.protocol_type {
                picker.set_protocol_type(protocol_type);
            }

            picker
        },
        Err(e) => {
            tracing::warn!(
                "Failed to setup image previews (falling back to halfblock rendering): {e}"
            );
            Picker::halfblocks()
        },
    }
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
    kind: PreviewKind,
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
    locked.application.previews.insert(key, kind, status);
}
