#!/bin/bash
#
# This is my dotfiles setup script. It is invoked by coder.com instances.
# We have this shared part of the setup, in setup.py, so let's run that first.
set -ex

MY_PATH=$(dirname "$0")
cd $MY_PATH

# Run the basic setup.
python3 "./setup.py"

# WTF
sudo apt update
sudo apt install -y wget fish java-common

# Temporary until we get java in the image or java in the WORKSPACE
# wget -O- https://apt.corretto.aws/corretto.key | sudo apt-key add -
# sudo add-apt-repository 'deb https://apt.corretto.aws stable main'


# # OK this stuff here is better in bash, and also is specific to setting up
# # coder.com instances, so. Add packages that I want in my coder image.
# sudo apt-get update
# sudo apt-get install -y java-1.8.0-amazon-corretto-jdk

# NOTE: THIS NEEDS TO BE INSTALLED IN THE BASE IMAGE OR CLANG DON'T RUN
# sudo apt-get install -y libtinfo5

# Change my shell to fish.
sudo chsh -s /usr/bin/fish $USER

# Make sure that the gitconfig that's on my computer links to my shared
# config.
git config --global include.path .gitconfig.shared

# Install local rust so I have rustfmt at the very least.
# (This should be in the image I think.)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Install bazelisk
# wget https://github.com/bazelbuild/bazelisk/releases/download/v1.12.0/bazelisk-linux-amd64
# mv ./bazelisk-linux-amd64 ~/bin/bazelisk
# chmod a+x ~/bin/bazelisk
