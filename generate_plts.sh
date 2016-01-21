#!/bin/sh

pushd . &> /dev/null

cd $1

WH=`which dialyzer`
echo "Dialyzer = $WH"

DEPS='deps/*/ebin'
echo "deps = $DEPS"
APPS="apps/*/ebin"
echo "apps = $APPS"

echo "Generating deps..."
dialyzer --build_plt --output_plt .deps.plt deps/*/ebin

echo "Generating apps plt.."
dialyzer --build_plt --output_plt .project.plt apps/*/ebin

cp .*.plt ~

popd &> /dev/null
