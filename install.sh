#!/bin/bash
#
# This is my dotfiles setup script. It is invoked by coder.com instances.
# We have this shared part of the setup, in setup.py, so let's run that first.
MY_PATH=$(dirname "$0")
cd $MY_PATH

python3 "./setup.py"

# OK this stuff here is better in bash, and also is specific to setting up
# coder.com instances, so. Add packages that I want in my coder image.
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt-get update
sudo apt-get install -y fish emacs27-nox tmux

# NOTE: THIS NEEDS TO BE INSTALLED IN THE BASE IMAGE
sudo apt-get install -y libtinfo5

# Change my shell to fish.
sudo chsh -s /usr/bin/fish $USER

# Make sure that the gitconfig that's on my computer links to my shared
# config.
git config --global include.path .gitconfig.shared

# Install local rust
# (This should be in the image I think.)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
