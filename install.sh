#!/bin/bash
#
# This is my dotfiles setup script. It is invoked by coder.com instances.
# We have this shared part of the setup, in setup.py, so let's run that first.
set -ex

MY_PATH=$(dirname "$0")
cd $MY_PATH

# Run the basic setup.
python3 "./setup.py"

# # OK this stuff here is better in bash, and also is specific to setting up
# # coder.com instances, so. Add packages that I want in my coder image.
sudo apt update
sudo apt install -y wget fish java-common atop htop

# Change my shell to fish.
sudo chsh -s /usr/bin/fish $USER

# Install fish related stuff
/usr/bin/fish "./setup.fish"

# Make sure that the gitconfig that's on my computer links to my shared
# config.
git config --global include.path .gitconfig.shared

# Install blacken and prettier
pip3 install --index-url=https://pypi.org/simple black
npm install -g prettier
