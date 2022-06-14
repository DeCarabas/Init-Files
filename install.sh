#!/bin/bash
#
# This is my dotfiles setup script. It is invoked by coder.com instances.
# We have this shared part of the setup, in setup.py, so let's run that first.
MY_PATH=$(dirname "$0")
cd $MY_PATH

python3 "./setup.py"

# OK this stuff here is better in bash, and also is specific to setting up
# coder.com instances, so.
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt-get update
sudo apt-get install -y fish emacs27-nox tmux

sudo chsh -s /usr/bin/fish $USER
