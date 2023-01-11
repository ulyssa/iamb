# iamb

[![Build Status](https://github.com/ulyssa/iamb/workflows/CI/badge.svg)](https://github.com/ulyssa/iamb/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/iamb.svg?logo=apache)](https://crates.io/crates/iamb)
[![Latest Version](https://img.shields.io/crates/v/iamb.svg?logo=rust)](https://crates.io/crates/iamb)

## About

`iamb` is a Matrix client for the terminal that uses Vim keybindings.

This project is a work-in-progress, and there's still a lot to be implemented,
but much of the basic client functionality is already present.

## Documentation

You can find documentation for installing, configuring, and using iamb on its
website, [iamb.chat].

## Installation

Install Rust and Cargo, and then run:

```
cargo install iamb
```

## Configuration

You can create a basic configuration in `$CONFIG_DIR/iamb/config.json` that looks like:

```json
{
    "profiles": {
        "example.com": {
            "url": "https://example.com",
            "user_id": "@user:example.com"
        }
    }
}
```

## Comparison With Other Clients

To get an idea of what is and isn't yet implemented, here is a subset of the
Matrix website's [features comparison table][client-comparison-matrix], showing
two other TUI clients and Element Web:

|                                         | iamb               | [gomuks]           | [weechat-matrix]   | Element Web/Desktop |
| --------------------------------------- | :----------------- | :----------------: | :----------------: | :-----------------: |
| Room directory                          | :x: ([#14])        | :x:                | :heavy_check_mark: | :heavy_check_mark:  |
| Room tag showing                        | :x: ([#15])        | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Room tag editing                        | :x: ([#15])        | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Search joined rooms                     | :x: ([#16])        | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Room user list                          | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Display Room Description                | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Edit Room Description                   | :heavy_check_mark: | :x:                | :heavy_check_mark: | :heavy_check_mark:  |
| Highlights                              | :x: ([#8])         | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Pushrules                               | :x:                | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Send read markers                       | :x: ([#11])        | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Display read markers                    | :x: ([#11])        | :x:                | :x:                | :heavy_check_mark:  |
| Sending Invites                         | :x: ([#7])         | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Accepting Invites                       | :x: ([#7])         | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Typing Notification                     | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| E2E                                     | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Replies                                 | :x: ([#3])         | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Attachment uploading                    | :heavy_check_mark: | :x:                | :heavy_check_mark: | :heavy_check_mark:  |
| Attachment downloading                  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Send stickers                           | :x:                | :x:                | :x:                | :heavy_check_mark:  |
| Send formatted messages (markdown)      | :x: ([#10])        | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Rich Text Editor for formatted messages | :x:                | :x:                | :x:                | :heavy_check_mark:  |
| Display formatted messages              | :x: ([#10])        | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Redacting                               | :x: ([#5])         | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
| Multiple Matrix Accounts                | :heavy_check_mark: | :x:                | :heavy_check_mark: | :x:                 |
| New user registration                   | :x:                | :x:                | :x:                | :heavy_check_mark:  |
| VOIP                                    | :x:                | :x:                | :x:                | :heavy_check_mark:  |
| Reactions                               | :x: ([#2])         | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Message editing                         | :x: ([#4])         | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Room upgrades                           | :x:                | :heavy_check_mark: | :x:                | :heavy_check_mark:  |
| Localisations                           | :x:                | 1                  | :x:                | 44                  |
| SSO Support                             | :x:                | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:  |
                                                                                       
## License

iamb is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/iamb/blob/master/LICENSE
[client-comparison-matrix]: https://matrix.org/clients-matrix/
[iamb.chat]: https://iamb.chat
[gomuks]: https://github.com/tulir/gomuks
[weechat-matrix]: https://github.com/poljar/weechat-matrix
[#2]: https://github.com/ulyssa/iamb/issues/2
[#3]: https://github.com/ulyssa/iamb/issues/3
[#4]: https://github.com/ulyssa/iamb/issues/4
[#5]: https://github.com/ulyssa/iamb/issues/5
[#6]: https://github.com/ulyssa/iamb/issues/6
[#7]: https://github.com/ulyssa/iamb/issues/7
[#8]: https://github.com/ulyssa/iamb/issues/8
[#9]: https://github.com/ulyssa/iamb/issues/9
[#10]: https://github.com/ulyssa/iamb/issues/10
[#11]: https://github.com/ulyssa/iamb/issues/11
[#12]: https://github.com/ulyssa/iamb/issues/12
[#13]: https://github.com/ulyssa/iamb/issues/13
[#14]: https://github.com/ulyssa/iamb/issues/14
[#15]: https://github.com/ulyssa/iamb/issues/15
[#16]: https://github.com/ulyssa/iamb/issues/16
