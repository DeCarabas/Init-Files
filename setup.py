#!/usr/local/bin/python3
import os

ignore = {'setup.cmd', 'setup.py', 'setup.ps1', 'readme.md', '.git'}

home = os.path.expanduser('~')
source_files = [file for file in os.listdir(os.getcwd()) if file not in ignore]
for source in source_files:
    source = os.path.abspath(source)
    dst = os.path.join(home, os.path.split(source)[1])
    if os.path.exists(dst):
        if os.path.islink(dst):
            other_src = os.readlink(dst)
            other_src = os.path.join(os.path.dirname(dst), other_src)
            other_src = os.path.abspath(other_src)
            if other_src != source:
                print(source)
                print(
                    'WARNING: {} is symlink but not into '
                    'Init-Files (it links to {})'.format(dst, other_src)
                )
        else:
            print('WARNING: {} already exists and is not a symlink'.format(dst))
        continue

    print('Linking: {} -> {}'.format(source, dst))
    os.symlink(source, dst)
