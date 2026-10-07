#!/bin/sh
# rustfmt wrapper used to format the rust-axum samples.
#
# The generator runs `rustfmt --edition 2024`, which also applies the 2024 style edition
# (import sorting, trailing semicolons, ...). The committed samples use the 2021 style,
# so regenerate them with:
#
#   RUST_POST_PROCESS_FILE=$PWD/samples/server/petstore/rust-axum/rustfmt-2021.sh \
#     ./bin/generate-samples.sh bin/configs/manual/rust-axum*
exec rustfmt --edition 2024 --style-edition 2021 "$@"
