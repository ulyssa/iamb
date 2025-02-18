<div align="center">
    <h1><img width="200" height="200" src="docs/iamb.svg"></h1>

[![Build Status](https://github.com/ulyssa/iamb/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/iamb/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/iamb.svg?logo=apache)][crates-io-iamb]
[![#iamb:0x.badd.cafe](https://img.shields.io/badge/matrix-%23iamb:0x.badd.cafe-blue)](https://matrix.to/#/#iamb:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/iamb.svg?logo=rust)][crates-io-iamb]
[![iamb](https://snapcraft.io/iamb/badge.svg)](https://snapcraft.io/iamb)

![Example Usage](https://iamb.chat/static/images/iamb-demo.gif)

</div>

## About

`iamb` is a Matrix client for the terminal that uses Vim keybindings. It includes support for:

- Threads, spaces, E2EE, and read receipts
- Image previews in terminals that support it (sixels, Kitty, and iTerm2), or using pixelated blocks for those that don't
- Notifications via terminal bell or desktop environment
- Send Markdown, HTML or plaintext messages
- Creating, joining, and leaving rooms
- Sending and accepting room invitations
- Editing, redacting, and reacting to messages
- Custom keybindings
- Multiple profiles

_You may want to [see this page as it was when the latest version was published][crates-io-iamb]._

## Documentation

You can find documentation for installing, configuring, and using iamb on its
website, [iamb.chat].

## Configuration

You can create a basic configuration in `$CONFIG_DIR/iamb/config.toml` that looks like:

```toml
[profiles."example.com"]
user_id = "@user:example.com"
```

If you homeserver is located on a different domain than the server part of the
`user_id` and you don't have a [`/.well-known`][well_known_entry] entry, then
you can explicitly specify the homeserver URL to use:

```toml
[profiles."example.com"]
url = "https://example.com"
user_id = "@user:example.com"
```

## Installation (via `crates.io`)

Install Rust (1.76.0 or above) and Cargo, and then run:

```
cargo install --locked iamb
```

See [Configuration](#configuration) for getting a profile set up.

## Installation (via package managers)

### Arch Linux

On Arch Linux a [package](https://aur.archlinux.org/packages/iamb-git) is available in the
Arch User Repositories (AUR). To install it simply run with your favorite AUR helper:

```
paru iamb-git
```

### FreeBSD

On FreeBSD a package is available from the official repositories. To install it simply run:

```
pkg install iamb
```

### macOS

On macOS a [package](https://formulae.brew.sh/formula/iamb#default) is availabe in Homebrew's
repository. To install it simply run:

```
brew install iamb
```

### NetBSD

On NetBSD a package is available from the official repositories. To install it simply run:

```
pkgin install iamb
```

### Nix / NixOS (flake)

```
nix profile install "github:ulyssa/iamb"
```

### openSUSE Tumbleweed

On openSUSE Tumbleweed a [package](https://build.opensuse.org/package/show/openSUSE:Factory/iamb) is available from the official repositories. To install it simply run:

```
zypper install iamb
```

### Snap

A snap for Linux distributions which [support](https://snapcraft.io/docs/installing-snapd) the packaging system.

```
snap install iamb
```

## License

iamb is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/iamb/blob/master/LICENSE
[crates-io-iamb]: https://crates.io/crates/iamb
[iamb.chat]: https://iamb.chat
[well_known_entry]: https://spec.matrix.org/latest/client-server-api/#getwell-knownmatrixclient
