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

wget --no-check-certificate https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el -O google-c-style.el
wget --no-check-certificate https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el -O use-theme.el
wget --no-check-certificate https://raw.githubusercontent.com/dwijnand/sbt-extras/master/sbt -O sbt
wget --no-check-certificate https://raw.githubusercontent.com/chenyanming/nov-xwidget/main/nov-xwidget.el -O nov-xwidget.el

if [ ! -d "pdf-tools" ]; then
    #git clone --branch pdf-roll https://github.com/dalanicolai/pdf-tools.git
    #git clone --branch continuous-scroll https://github.com/aikrahguzar/pdf-tools.git
    git clone --branch child-frame-preview https://github.com/aikrahguzar/pdf-tools.git
fi

build_pdf_tools pdf-tools

#wget https://raw.githubusercontent.com/dalanicolai/image-roll.el/main/image-roll.el -O pdf-tools/lisp/image-roll.el
#wget --no-check-certificate https://raw.githubusercontent.com/aikrahguzar/image-roll.el/main/image-roll.el -O pdf-tools/lisp/image-roll.el
wget --no-check-certificate https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el -O pdf-tools/lisp/pdf-view-restore.el
wget --no-check-certificate https://raw.githubusercontent.com/jdtsmith/eglot-booster/main/eglot-booster.el -O eglot-booster.el
