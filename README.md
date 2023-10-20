# iamb

[![Build Status](https://github.com/ulyssa/iamb/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/iamb/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/iamb.svg?logo=apache)](https://crates.io/crates/iamb)
[![#iamb:0x.badd.cafe](https://img.shields.io/badge/matrix-%23iamb:0x.badd.cafe-blue)](https://matrix.to/#/#iamb:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/iamb.svg?logo=rust)](https://crates.io/crates/iamb)

## About

`iamb` is a Matrix client for the terminal that uses Vim keybindings.

This project is a work-in-progress, and there's still a lot to be implemented,
but much of the basic client functionality is already present.

![Example Usage](https://iamb.chat/static/images/iamb-demo.gif)

## Documentation

You can find documentation for installing, configuring, and using iamb on its
website, [iamb.chat].

## Installation

Install Rust (1.67.0 or above) and Cargo, and then run:

```
cargo install --locked iamb
```

### NetBSD

On NetBSD a package is available from the official repositories. To install it simply run:

```
pkgin install iamb
```

### Arch Linux

On Arch Linux a [package](https://aur.archlinux.org/packages/iamb-git) is available in the
Arch User Repositories (AUR). To install it simply run with your favorite AUR helper:

```
paru iamb-git
```

### Nix / NixOS (flake)

```
nix profile install "github:ulyssa/iamb"
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

|                                         | iamb        | [gomuks] | [weechat-matrix] | Element Web/Desktop |
| --------------------------------------- | :---------- | :------: | :--------------: | :-----------------: |
| Room directory                          | ❌  ([#14]) | ❌       | ✔️                | ✔️                   |
| Room tag showing                        | ✔️           | ✔️        | ❌               | ✔️                   |
| Room tag editing                        | ✔️           | ✔️        | ❌               | ✔️                   |
| Search joined rooms                     | ❌  ([#16]) | ✔️        | ❌               | ✔️                   |
| Room user list                          | ✔️           | ✔️        | ✔️                | ✔️                   |
| Display Room Description                | ✔️           | ✔️        | ✔️                | ✔️                   |
| Edit Room Description                   | ✔️           | ❌       | ✔️                | ✔️                   |
| Highlights                              | ❌  ([#8])  | ✔️        | ✔️                | ✔️                   |
| Pushrules                               | ❌          | ✔️        | ❌               | ✔️                   |
| Send read markers                       | ✔️           | ✔️        | ✔️                | ✔️                   |
| Display read markers                    | ✔️           | ❌       | ❌               | ✔️                   |
| Sending Invites                         | ✔️           | ✔️        | ✔️                | ✔️                   |
| Accepting Invites                       | ✔️           | ✔️        | ✔️                | ✔️                   |
| Typing Notification                     | ✔️           | ✔️        | ✔️                | ✔️                   |
| E2E                                     | ✔️           | ✔️        | ✔️                | ✔️                   |
| Replies                                 | ✔️           | ✔️        | ❌               | ✔️                   |
| Attachment uploading                    | ✔️           | ❌       | ✔️                | ✔️                   |
| Attachment downloading                  | ✔️           | ✔️        | ✔️                | ✔️                   |
| Send stickers                           | ❌          | ❌       | ❌               | ✔️                   |
| Send formatted messages (markdown)      | ✔️           | ✔️        | ✔️                | ✔️                   |
| Rich Text Editor for formatted messages | ❌          | ❌       | ❌               | ✔️                   |
| Display formatted messages              | ✔️           | ✔️        | ✔️                | ✔️                   |
| Redacting                               | ✔️           | ✔️        | ✔️                | ✔️                   |
| Multiple Matrix Accounts                | ✔️           | ❌       | ✔️                | ❌                  |
| New user registration                   | ❌          | ❌       | ❌               | ✔️                   |
| VOIP                                    | ❌          | ❌       | ❌               | ✔️                   |
| Reactions                               | ✔️           | ✔️        | ❌               | ✔️                   |
| Message editing                         | ✔️           | ✔️        | ❌               | ✔️                   |
| Room upgrades                           | ❌ ([#41])  | ✔️        | ❌               | ✔️                   |
| Localisations                           | ❌          | 1        | ❌               | 44                  |
| SSO Support                             | ❌          | ✔️        | ✔️                | ✔️                   |
                                                                                       
## License

iamb is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/iamb/blob/master/LICENSE
[client-comparison-matrix]: https://matrix.org/clients-matrix/
[iamb.chat]: https://iamb.chat
[gomuks]: https://github.com/tulir/gomuks
[weechat-matrix]: https://github.com/poljar/weechat-matrix
[#8]: https://github.com/ulyssa/iamb/issues/8
[#14]: https://github.com/ulyssa/iamb/issues/14
[#16]: https://github.com/ulyssa/iamb/issues/16
[#41]: https://github.com/ulyssa/iamb/issues/41
