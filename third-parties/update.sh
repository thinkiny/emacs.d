#!/bin/bash

set -euoE pipefail

# Error handler to show the failed command
error_handler() {
    local line_no=$1
    local last_command=$2
    echo "Error on line $line_no: Command '$last_command' failed."
    exit 1
}

# Set up error handler
trap 'error_handler $LINENO "$BASH_COMMAND"' ERR

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ELPA_DIR="${HOME}/.emacs.d/elpa"

# Repositories to download
FILE_URLS=(
    # "https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el"
    # "https://raw.githubusercontent.com/jdtsmith/eglot-booster/main/eglot-booster.el"
    "https://raw.githubusercontent.com/martinbaillie/vterm-anti-flicker-filter/refs/heads/main/vterm-anti-flicker-filter.el"
    "https://codeberg.org/slotThe/eglot-hover/raw/branch/main/eglot-hover.el"
    "https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el"
    "https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el"
    "https://raw.githubusercontent.com/gaoDean/org-remoteimg/main/org-remoteimg.el"
    "https://raw.githubusercontent.com/abo-abo/org-download/refs/heads/master/org-download.el"
    "https://codeberg.org/pranshu/haskell-ts-mode/raw/branch/main/haskell-ts-mode.el"
)

# Build PDF tools
build_pdf_tools() {
    local repo_dir="${SCRIPT_DIR}/pdf-tools"

    if [ ! -d "$repo_dir" ]; then
        git clone --branch child-frame-preview https://github.com/aikrahguzar/pdf-tools.git "$repo_dir"
    fi

    pushd "$repo_dir"
    git pull
    pushd server
    ./autogen.sh
    ./configure
    rm -f epdfinfo
    make -j4
    cp epdfinfo ../lisp/
    popd
    popd

    wget --no-check-certificate https://raw.githubusercontent.com/orgtre/qpdf.el/refs/heads/master/qpdf.el -O qpdf.el
}

# Update LSP multiplexer
update_lsp_multiplexer() {
    local repo_dir="${SCRIPT_DIR}/lsp-multiplexer"

    if [ ! -d "$repo_dir" ]; then
        git clone --depth 1 https://github.com/garyo/lsp-multiplexer.git "$repo_dir"
    else
        pushd "$repo_dir"
        git pull
        popd
    fi
}

# Update any git repository
update_repo() {
    pushd "$1"
    git pull
    popd
}

# Update LSP tools
update_lsp() {
    go install golang.org/x/tools/gopls@latest
    pipx upgrade pyrefly
    pipx upgrade rassumfrassum
}

# Download all configured files
update_files() {
    for url in "${FILE_URLS[@]}"; do
        output=$(basename "$url")
        echo "Downloading: $url -> $output"

        local retry_count=0
        local max_retries=5
        while [[ $retry_count -lt $max_retries ]]; do
            if wget -q --no-check-certificate "$url" -O "$output"; then
                break
            else
                ((retry_count++))
                if [[ $retry_count -lt $max_retries ]]; then
                    # local wait_time=$((2 ** retry_count))  # 2, 4, 8 seconds
                    local wait_time=1
                    echo "⚠ Attempt $retry_count failed, retrying in ${wait_time}s... ($((max_retries - retry_count)) attempts left)"
                    sleep $wait_time
                else
                    echo "✗ Failed to download $output after $max_retries attempts"
                    return 1
                fi
            fi
        done
    done
}

# Main function
main() {
    cd "$SCRIPT_DIR"

    # Check for command line arguments
    if [[ $# -eq 0 ]]; then
        # No arguments - run full update
        echo "Running full update..."

        # Download files
        update_files

        # Optional operations (commented out by default)
        # build_pdf_tools
        # update_lsp_multiplexer

        # Update repositories
        update_lsp
        update_repo "${ELPA_DIR}/claude-code-ide"
        update_repo "${ELPA_DIR}/tramp-rpc"
    else
        case $1 in
            lsp)
                update_lsp
                ;;
            repo)
                update_repo "${ELPA_DIR}/claude-code-ide"
                update_repo "${ELPA_DIR}/tramp-rpc"
                ;;
            *)
                echo "Unknown option: $1"
                exit 1
                ;;
        esac
    fi
}

# Run main function
main "$@"
