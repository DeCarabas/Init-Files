#!/usr/local/bin/python3
import os

ignore = {
    "setup.cmd",
    "setup.py",
    "setup.ps1",
    "readme.md",
    ".git",
    "vscode",
    ".DS_Store",
}


def link_helper(source, dst):
    """A little helper to symlink source to destination, that prints a warning
    if the destination exists and is not a link into our Init-Files repository.
    """
    if os.path.exists(dst):
        if os.path.islink(dst):
            other_src = os.readlink(dst)
            other_src = os.path.join(os.path.dirname(dst), other_src)
            other_src = os.path.abspath(other_src)
            if other_src != source:
                print(source)
                print(
                    "WARNING: {} is symlink but not into "
                    "Init-Files (it links to {})".format(dst, other_src)
                )
        else:
            print("WARNING: {} already exists and is not a symlink".format(dst))
    else:
        print("Linking: {} -> {}".format(source, dst))
        os.symlink(source, dst)


home = os.path.expanduser("~")
source_files = [file for file in os.listdir(os.getcwd()) if file not in ignore]
for source in source_files:
    source = os.path.abspath(source)
    dst = os.path.join(home, os.path.split(source)[1])
    link_helper(source, dst)

# VS Code goes somewhere else, not in the home directory. It depends on the
# platform and it also has this VS Code @ FB thing, yikes. So here are the
# roots, in the order that we're going to prefer them.
vscode_source = os.path.abspath("vscode")
possible_vscode_roots = [
    os.path.join(home, "Library", "Application Support"),  # MacOS X
    os.getenv("APPDATA"),  # Windows
    os.path.join(home, ".config"),  # Everywhere
]
existing_vscode_roots = [
    root for root in possible_vscode_roots if root and os.path.exists(root)
]
if existing_vscode_roots:
    vscode_root = existing_vscode_roots[0]
    for variant in ["Code", "VS Code @ FB"]:
        dst_root = os.path.join(vscode_root, variant)
        if not os.path.exists(dst_root):
            os.mkdir(dst_root)
        link_helper(vscode_source, os.path.join(dst_root, "User"))
else:
    print(
        "WARNING: No viable root for VS Code config (tried {})".format(
            possible_vscode_roots
        )
    )
