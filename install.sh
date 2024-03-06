#!/bin/bash
#
# This is my dotfiles setup script. It is invoked by coder.com instances.
# We have this shared part of the setup, in setup.py, so let's run that first.
set -ex

MY_PATH=$(dirname "$0")
cd $MY_PATH

# Run the basic setup.
python3 "./setup.py"

bash -i -l -c "nvm install 18"

# Run the coder setup.
python3 "./coder-setup.py"
