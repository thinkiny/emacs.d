#!/bin/bash

set -euxo pipefail

function build_pdf_tools() {
    if [ ! -d "pdf-tools" ]; then
        #git clone --branch pdf-roll https://github.com/dalanicolai/pdf-tools.git
        #git clone --branch continuous-scroll https://github.com/aikrahguzar/pdf-tools.git
        git clone --branch child-frame-preview https://github.com/aikrahguzar/pdf-tools.git
    fi
    pushd $1
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

function update_lsp_multiplexer() {
    if [ ! -d "lsp-multiplexer" ]; then
        git clone --depth 1 https://github.com/garyo/lsp-multiplexer.git
    else
        pushd lsp-multiplexer
        git pull
        popd
    fi
}

function update_repo() {
    pushd $1
    git pull
    popd
}

function update_lsp() {
    go install golang.org/x/tools/gopls@latest
    #npm update -g
    pipx upgrade pyrefly
    pipx upgrade rassumfrassum
}

wget --no-check-certificate https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el -O google-c-style.el
wget --no-check-certificate https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el -O use-theme.el

# build_pdf_tools pdf-tools
wget --no-check-certificate https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el -O pdf-tools/lisp/pdf-view-restore.el
wget --no-check-certificate https://raw.githubusercontent.com/jdtsmith/eglot-booster/main/eglot-booster.el -O eglot-booster.el
wget --no-check-certificate https://raw.githubusercontent.com/gaoDean/org-remoteimg/main/org-remoteimg.el -O org-remoteimg.el
wget --no-check-certificate https://raw.githubusercontent.com/abo-abo/org-download/refs/heads/master/org-download.el -O org-download.el
wget --no-check-certificate https://codeberg.org/pranshu/haskell-ts-mode/raw/branch/main/haskell-ts-mode.el -O haskell-ts-mode.el
#wget --no-check-certificate https://raw.githubusercontent.com/bobrowadam/.emacs.d/refs/heads/main/modules/eglot-sqls.el  -O eglot-sqls.el
#wget --no-check-certificate https://raw.githubusercontent.com/garyo/lsp-multiplexer/refs/heads/main/lsp_multiplexer.py -O lsp_multiplexer.py
wget --no-check-certificate https://codeberg.org/slotThe/eglot-hover/raw/branch/main/eglot-hover.el -O eglot-hover.el

# update-repo
# update_lsp_multiplexer
update_repo ~/.emacs.d/elpa/claude-code-ide/
update_lsp
