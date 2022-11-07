#!/bin/bash

set -euxo pipefail

wget https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el -O google-c-style.el
wget https://raw.githubusercontent.com/canatella/use-theme/master/use-theme.el -O use-theme.el
wget https://raw.githubusercontent.com/dwijnand/sbt-extras/master/sbt -O sbt
wget https://raw.githubusercontent.com/chenyanming/nov-xwidget/main/nov-xwidget.el -O nov-xwidget.el

if [ -d "pdf-tools" ]; then
    pushd pdf-tools
    git pull
    pushd server
    ./autogen.sh
    ./configure
    make -j4
    cp epdfinfo ../lisp/
    popd
    popd
else
    git clone --branch pdf-roll https://github.com/dalanicolai/pdf-tools.git
fi

wget https://raw.githubusercontent.com/dalanicolai/image-roll.el/main/image-roll.el -O pdf-tools/lisp/image-roll.el
wget https://raw.githubusercontent.com/007kevin/pdf-view-restore/master/pdf-view-restore.el -O pdf-tools/lisp/pdf-view-restore.el
