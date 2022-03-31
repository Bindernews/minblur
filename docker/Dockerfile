# syntax=docker/dockerfile:1.4
FROM rust:1.59
ENV RUSTUP_TOOLCHAIN=nightly \
    LLVM_PROFILE_FILE='coverage-%p-%m.profraw'
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/usr/local/rustup/downloads \
    rustup component add llvm-tools-preview &&\
    rustup toolchain install nightly &&\
    cargo install grcov &&\
    apt update &&\
    apt install -y vim valgrind jq lcov
WORKDIR /usr/local/bin
RUN <<EOF1 cat >run-coverage.sh &&\
    <<EOF2 cat >setup-env.sh &&\
    <<EOF3 cat >start-dhat-web.sh &&\
    chmod ugo+x run-coverage.sh setup-env.sh start-dhat-web.sh
#!/bin/bash
POSITIONAL_ARGS=()
SOURCE_DIR="."
FORMAT=""
COVERAGE_DIR="/target/debug/coverage/"
OUTPUT_NAME=""
# Add default arguments
set -- -t lcov \$*
while [[ \$# -gt 0 ]]; do
  case \$1 in
    -s)
      SOURCE_DIR="\$2"
      shift; shift;
      ;;
    -t|--format)
      FORMAT="\$2"
      case "\$FORMAT" in
        html)
          OUTPUT_PATH=""
          ;;
        lcov)
          OUTPUT_NAME="coverage.info"
          ;;
        *)
          echo "ERROR: invalid output type '\$FORMAT'" >&2
          exit 1
          ;;
      esac
      shift; shift;
      ;;
    *)
      POSITIONAL_ARGS+=("\$1")
      shift
      ;;
  esac
done
# Re-assign positional args
set -- \${POSITIONAL_ARGS[@]}
COVERAGE_DIR="\${SOURCE_DIR}/\${COVERAGE_DIR}"
mkdir -p "\${COVERAGE_DIR}"
grcov "\$SOURCE_DIR" -s "\$SOURCE_DIR" --binary-path "\$SOURCE_DIR/target/debug/" -t "\$FORMAT" \
  --branch --ignore-not-existing -o "\${COVERAGE_DIR}/\${OUTPUT_NAME}" \
  --ignore "src/common/tuple_helper.rs" \
  --ignore "src/common/mod.rs"
if [ "\$FORMAT" == "lcov" ]; then
  INFO_FILE="\${COVERAGE_DIR}/\${OUTPUT_NAME}"
  mv "\$INFO_FILE" "\${INFO_FILE}2"
  lcov -q --extract "\${INFO_FILE}2" "src/**" -o "\${INFO_FILE}" --rc lcov_branch_coverage=1
  genhtml "\${INFO_FILE}" -o "\${COVERAGE_DIR}" --branch-coverage
fi
EOF1
#!/bin/bash
export RUSTFLAGS="-C instrument-coverage"
EOF2
pushd /usr/libexec/valgrind
python3 -m http.server
popd
EOF3
WORKDIR /

# Usage in git-bash:
# docker run -it --rm -v "/$(pwd)://src/" -v "cargo_registry://usr/local/cargo/registry" rust-coverage
# Container commands:
# > cd /src && source setup-env.sh
# > cargo test
# > run-coverage.sh

