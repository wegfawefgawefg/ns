#!/bin/bash

clear
# print now executing cargo build
echo "Now executing cargo build"
cargo build
# print now executing cargo run
echo "Now executing cargo run"
cargo run --bin ns
# print now executing cargo test
echo "Now executing cargo test"
cargo test -- --nocapture