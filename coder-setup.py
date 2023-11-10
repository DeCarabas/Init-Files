import os
import pathlib
import subprocess
import urllib.request


def run(*args):
    subprocess.run(args, check=True)


def install_packages():
    packages = [
        "atop",
        "fish",
        "htop",
        "java-common",
        "unixodbc-dev",
        "wget",
    ]

    run("sudo", "apt", "update")
    run("sudo", "apt", "install", "-y", *packages)


def change_shell():
    run("sudo", "chsh", "-s", "/usr/bin/fish", os.environ["USER"])
    run("/usr/bin/fish", "./setup.fish")


def configure_git():
    run("git", "config", "--global", "include.path", ".gitconfig.shared")


def backup_pip():
    pip = pathlib.Path.home() / ".config" / "pip"
    if pip.exists():
        pip.rename("pip.bak")


def restore_pip():
    pip = pathlib.Path.home() / ".config" / "pip.bak"
    if pip.exists():
        pip.rename("pip")


def configure_python():
    backup_pip()
    try:
        run("pip3", "install", "black")
        run("sudo", "npm", "install", "-g", "prettier", "pyright")

        installer = urllib.request.urlopen("https://install.python-poetry.org").read()
        subprocess.run(["python3", "-"], input=installer, check=True)
    finally:
        restore_pip()


install_packages()
change_shell()
configure_git()
configure_python()
