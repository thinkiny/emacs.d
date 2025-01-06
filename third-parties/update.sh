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

wget --no-check-certificate https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el -O google-c-style.el
wget --no-check-certificate https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el -O use-theme.el

# build_pdf_tools pdf-tools
wget --no-check-certificate https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el -O pdf-tools/lisp/pdf-view-restore.el
wget --no-check-certificate https://raw.githubusercontent.com/jdtsmith/eglot-booster/main/eglot-booster.el -O eglot-booster.el
wget --no-check-certificate https://raw.githubusercontent.com/gaoDean/org-remoteimg/main/org-remoteimg.el -O org-remoteimg.el
wget --no-check-certificate https://raw.githubusercontent.com/abo-abo/org-download/refs/heads/master/org-download.el -O org-download.el
wget --no-check-certificate https://codeberg.org/pranshu/haskell-ts-mode/raw/branch/main/haskell-ts-mode.el -O haskell-ts-mode.el
