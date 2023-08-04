#!/bin/bash

set -euxo pipefail

function build_pdf_tools() {
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
}

wget https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el -O google-c-style.el
wget https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el -O use-theme.el
wget https://raw.githubusercontent.com/dwijnand/sbt-extras/master/sbt -O sbt
wget https://raw.githubusercontent.com/chenyanming/nov-xwidget/main/nov-xwidget.el -O nov-xwidget.el

if [ ! -d "pdf-tools" ]; then
    git clone --branch pdf-roll https://github.com/dalanicolai/pdf-tools.git
fi

build_pdf_tools pdf-tools

wget https://raw.githubusercontent.com/dalanicolai/image-roll.el/main/image-roll.el -O pdf-tools/lisp/image-roll.el
wget https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el -O pdf-tools/lisp/pdf-view-restore.el
wget https://raw.githubusercontent.com/KaranAhlawat/scala-ts-mode/main/scala-ts-mode.el -O scala-ts-mode.el
