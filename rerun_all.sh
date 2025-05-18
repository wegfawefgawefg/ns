#!/bin/bash

clear
# print now executing cargo build --release
echo "Now executing cargo build --release"
cargo build --release
# print now executing cargo run --release
echo "Now executing cargo run --release"
cargo run --bin ns --release 
# print now executing cargo test --release
echo "Now executing cargo test --release"
cargo test --release -- --nocapture
