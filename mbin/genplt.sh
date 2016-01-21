#!/bin/bash

pushd . &> /dev/null

cd $1

echo "Generating apps plt.."
dialyzer --build_plt --output_plt .project.plt apps/*/ebin

cp .*.plt ~

popd &> /dev/null
