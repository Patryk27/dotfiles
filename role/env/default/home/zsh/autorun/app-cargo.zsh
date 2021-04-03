alias ca='clear && cargo'

alias cab='clear && cargo build'
alias cabr='clear && cargo build --release'

alias cac='clear && cargo check'

alias caf='clear && cargo fmt'

alias car='clear && cargo run'
alias carb='clear && RUST_BACKTRACE=1 cargo run'
alias carr='clear && cargo run --release'
alias carrb='clear && RUST_BACKTRACE=1 cargo run --release'

alias cate='clear && cargo test --quiet'
alias cateb='clear && RUST_BACKTRACE=1 cargo test'

alias catew='clear && cargo test --quiet --workspace'
alias catewb='clear && RUST_BACKTRACE=1 cargo test --workspace'

alias catewf='clear && cargo test --all-features --quiet --workspace'
alias catewfb='clear && RUST_BACKTRACE=1 cargo test --all-features --workspace'

alias cau='clear && cargo update'
alias caup='clear && cargo update --package'
