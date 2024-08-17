//! Code for converting composed messages into content to send to the homeserver.
use comrak::{markdown_to_html, ComrakOptions};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::space0,
    combinator::value,
    IResult,
};

use matrix_sdk::ruma::events::room::message::{
    EmoteMessageEventContent,
    MessageType,
    RoomMessageEventContent,
    TextMessageEventContent,
};

#[derive(Clone, Debug, Default)]
enum SlashCommand {
    /// Send an emote message.
    Emote,

    /// Send a message as literal HTML.
    Html,

    /// Send a message without parsing any markup.
    Plaintext,

    /// Send a Markdown message (the default message markup).
    #[default]
    Markdown,

    /// Send a message with confetti effects in clients that show them.
    Confetti,

    /// Send a message with fireworks effects in clients that show them.
    Fireworks,

    /// Send a message with heart effects in clients that show them.
    Hearts,

    /// Send a message with rainfall effects in clients that show them.
    Rainfall,

    /// Send a message with snowfall effects in clients that show them.
    Snowfall,

    /// Send a message with heart effects in clients that show them.
    SpaceInvaders,
}

impl SlashCommand {
    fn to_message(&self, input: &str) -> anyhow::Result<MessageType> {
        let msgtype = match self {
            SlashCommand::Emote => {
                let msg = if let Some(html) = text_to_html(input) {
                    EmoteMessageEventContent::html(input, html)
                } else {
                    EmoteMessageEventContent::plain(input)
                };

                MessageType::Emote(msg)
            },
            SlashCommand::Html => {
                let msg = TextMessageEventContent::html(input, input);
                MessageType::Text(msg)
            },
            SlashCommand::Plaintext => {
                let msg = TextMessageEventContent::plain(input);
                MessageType::Text(msg)
            },
            SlashCommand::Markdown => {
                let msg = text_to_message_content(input.to_string());
                MessageType::Text(msg)
            },
            SlashCommand::Confetti => {
                MessageType::new("nic.custom.confetti", input.into(), Default::default())?
            },
            SlashCommand::Fireworks => {
                MessageType::new("nic.custom.fireworks", input.into(), Default::default())?
            },
            SlashCommand::Hearts => {
                MessageType::new("io.element.effect.hearts", input.into(), Default::default())?
            },
            SlashCommand::Rainfall => {
                MessageType::new("io.element.effect.rainfall", input.into(), Default::default())?
            },
            SlashCommand::Snowfall => {
                MessageType::new("io.element.effect.snowfall", input.into(), Default::default())?
            },
            SlashCommand::SpaceInvaders => {
                MessageType::new(
                    "io.element.effects.space_invaders",
                    input.into(),
                    Default::default(),
                )?
            },
        };

        Ok(msgtype)
    }
}

fn parse_slash_command_inner(input: &str) -> IResult<&str, SlashCommand> {
    let (input, _) = space0(input)?;
    let (input, slash) = alt((
        value(SlashCommand::Emote, tag("/me ")),
        value(SlashCommand::Html, tag("/h ")),
        value(SlashCommand::Html, tag("/html ")),
        value(SlashCommand::Plaintext, tag("/p ")),
        value(SlashCommand::Plaintext, tag("/plain ")),
        value(SlashCommand::Plaintext, tag("/plaintext ")),
        value(SlashCommand::Markdown, tag("/md ")),
        value(SlashCommand::Markdown, tag("/markdown ")),
        value(SlashCommand::Confetti, tag("/confetti ")),
        value(SlashCommand::Fireworks, tag("/fireworks ")),
        value(SlashCommand::Hearts, tag("/hearts ")),
        value(SlashCommand::Rainfall, tag("/rainfall ")),
        value(SlashCommand::Snowfall, tag("/snowfall ")),
        value(SlashCommand::SpaceInvaders, tag("/spaceinvaders ")),
    ))(input)?;
    let (input, _) = space0(input)?;

    Ok((input, slash))
}

fn parse_slash_command(input: &str) -> anyhow::Result<(&str, SlashCommand)> {
    match parse_slash_command_inner(input) {
        Ok(input) => Ok(input),
        Err(e) => Err(anyhow::anyhow!("Failed to parse slash command: {e}")),
    }
}

/// Check whether this character is not used for markup in Markdown.
///
/// Markdown uses just about every ASCII punctuation symbol in some way, especially
/// once autolinking is involved, so we really just check whether it's non-punctuation or
/// single/double quotations.
fn not_markdown_char(c: char) -> bool {
    if !c.is_ascii_punctuation() {
        return true;
    }

    matches!(c, '"' | '\'')
}

/// Check whether the input actually needs to be processed as Markdown.
fn not_markdown(input: &str) -> bool {
    input.chars().all(not_markdown_char)
}

fn text_to_html(input: &str) -> Option<String> {
    if not_markdown(input) {
        return None;
    }

    let mut options = ComrakOptions::default();
    options.extension.autolink = true;
    options.extension.shortcodes = true;
    options.extension.strikethrough = true;
    options.render.hardbreaks = true;
    markdown_to_html(input, &options).into()
}

fn text_to_message_content(input: String) -> TextMessageEventContent {
    if let Some(html) = text_to_html(input.as_str()) {
        TextMessageEventContent::html(input, html)
    } else {
        TextMessageEventContent::plain(input)
    }
}

pub fn text_to_message(input: String) -> RoomMessageEventContent {
    let msg = parse_slash_command(input.as_str())
        .and_then(|(input, slash)| slash.to_message(input))
        .unwrap_or_else(|_| MessageType::Text(text_to_message_content(input)));

    RoomMessageEventContent::new(msg)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_markdown_autolink() {
        let input = "http://example.com\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<p><a href=\"http://example.com\">http://example.com</a></p>\n"
        );

        let input = "www.example.com\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<p><a href=\"http://www.example.com\">www.example.com</a></p>\n"
        );

        let input = "See docs (they're at https://iamb.chat)\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<p>See docs (they're at <a href=\"https://iamb.chat\">https://iamb.chat</a>)</p>\n"
        );
    }

    #[test]
    fn test_markdown_message() {
        let input = "**bold**\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><strong>bold</strong></p>\n");

        let input = "*emphasis*\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><em>emphasis</em></p>\n");

        let input = "`code`\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><code>code</code></p>\n");

        let input = "```rust\nconst A: usize = 1;\n```\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<pre><code class=\"language-rust\">const A: usize = 1;\n</code></pre>\n"
        );

        let input = ":heart:\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p>\u{2764}\u{FE0F}</p>\n");

        let input = "para *1*\n\npara _2_\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<p>para <em>1</em></p>\n<p>para <em>2</em></p>\n"
        );

        let input = "line 1\nline ~~2~~\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p>line 1<br />\nline <del>2</del></p>\n");

        let input = "# Heading\n## Subheading\n\ntext\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<h1>Heading</h1>\n<h2>Subheading</h2>\n<p>text</p>\n"
        );
    }

    #[test]
    fn test_markdown_headers() {
        let input = "hello\n=====\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<h1>hello</h1>\n");

        let input = "hello\n-----\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<h2>hello</h2>\n");
    }

    #[test]
    fn test_markdown_lists() {
        let input = "- A\n- B\n- C\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<ul>\n<li>A</li>\n<li>B</li>\n<li>C</li>\n</ul>\n"
        );

        let input = "1) A\n2) B\n3) C\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<ol>\n<li>A</li>\n<li>B</li>\n<li>C</li>\n</ol>\n"
        );
    }

    #[test]
    fn test_no_markdown_conversion_on_simple_text() {
        let input = "para 1\n\npara 2\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert!(content.formatted.is_none());

        let input = "line 1\nline 2\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert!(content.formatted.is_none());

        let input = "isn't markdown\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert!(content.formatted.is_none());

        let input = "\"scare quotes\"\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert!(content.formatted.is_none());
    }

    #[test]
    fn text_to_message_slash_commands() {
        let MessageType::Text(content) = text_to_message("/html <b>bold</b>".into()).msgtype else {
            panic!("Expected MessageType::Text");
        };
        assert_eq!(content.body, "<b>bold</b>");
        assert_eq!(content.formatted.unwrap().body, "<b>bold</b>");

        let MessageType::Text(content) = text_to_message("/h <b>bold</b>".into()).msgtype else {
            panic!("Expected MessageType::Text");
        };
        assert_eq!(content.body, "<b>bold</b>");
        assert_eq!(content.formatted.unwrap().body, "<b>bold</b>");

        let MessageType::Text(content) = text_to_message("/plain <b>bold</b>".into()).msgtype
        else {
            panic!("Expected MessageType::Text");
        };
        assert_eq!(content.body, "<b>bold</b>");
        assert!(content.formatted.is_none(), "{:?}", content.formatted);

        let MessageType::Text(content) = text_to_message("/p <b>bold</b>".into()).msgtype else {
            panic!("Expected MessageType::Text");
        };
        assert_eq!(content.body, "<b>bold</b>");
        assert!(content.formatted.is_none(), "{:?}", content.formatted);

        let MessageType::Emote(content) = text_to_message("/me *bold*".into()).msgtype else {
            panic!("Expected MessageType::Emote");
        };
        assert_eq!(content.body, "*bold*");
        assert_eq!(content.formatted.unwrap().body, "<p><em>bold</em></p>\n");

        let content = text_to_message("/confetti hello".into()).msgtype;
        assert_eq!(content.msgtype(), "nic.custom.confetti");
        assert_eq!(content.body(), "hello");

        let content = text_to_message("/fireworks hello".into()).msgtype;
        assert_eq!(content.msgtype(), "nic.custom.fireworks");
        assert_eq!(content.body(), "hello");

        let content = text_to_message("/hearts hello".into()).msgtype;
        assert_eq!(content.msgtype(), "io.element.effect.hearts");
        assert_eq!(content.body(), "hello");

        let content = text_to_message("/rainfall hello".into()).msgtype;
        assert_eq!(content.msgtype(), "io.element.effect.rainfall");
        assert_eq!(content.body(), "hello");

        let content = text_to_message("/snowfall hello".into()).msgtype;
        assert_eq!(content.msgtype(), "io.element.effect.snowfall");
        assert_eq!(content.body(), "hello");

        let content = text_to_message("/spaceinvaders hello".into()).msgtype;
        assert_eq!(content.msgtype(), "io.element.effects.space_invaders");
        assert_eq!(content.body(), "hello");
    }
}
