# Notes For Package Maintainers

## Linking Against System Packages

The default Cargo features for __iamb__ will bundle SQLite and use [rustls] for
TLS. Package maintainers may want to link against the system's native SQLite
and TLS libraries instead. To do so, you'll want to build without the default
features and specify that it should build with `native-tls`:

```
% cargo build --release --no-default-features --features=native-tls
```

## Enabling LTO

Enabling LTO can result in smaller binaries. There is a separate profile to
enable it when building:

```
% cargo build --profile release-lto
```

Note that this [can fail][ring-lto] in some build environments if both Clang
and GCC are present.

## Documentation

In addition to the compiled binary, there are other files in the repo that
you'll want to install as part of a package:

| Repository Path      | Installed Path (may vary per OS)    |
| -------------------- | ----------------------------------- |
| /config.example.toml | /usr/share/iamb/config.example.toml |
| /docs/iamb.1         | /usr/share/man/man1/iamb.1          |
| /docs/iamb.5         | /usr/share/man/man5/iamb.5          |

[ring-lto]: https://github.com/briansmith/ring/issues/1444
[rustls]: https://crates.io/crates/rustls
