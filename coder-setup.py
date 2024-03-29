import os
import pathlib
import shutil
import subprocess
import tempfile
import urllib.request


ODBC_INI_CONTENTS = """
[snowflake]
Description=SnowflakeDB
Driver=SnowflakeDSIIDriver
Locale=en-US
PORT=443
SSL=on

[ODBC Data Sources]
snowflake = SnowflakeDSIIDriver
"""


def run(*args):
    subprocess.run(args, check=True)


def install_packages():
    packages = [
        "atop",
        "fish",
        "htop",
        "java-common",
        "odbcinst",
        "unixodbc",
        "unixodbc-dev",
        "wget",
        "earlyoom",
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
    pip_bak = pathlib.Path.home() / ".config" / "pip.bak"
    if pip_bak.exists():
        print("Removing stray backup directory...")
        shutil.rmtree(pip_bak)

    if pip.exists():
        pip.rename(pip_bak)


def restore_pip():
    pip = pathlib.Path.home() / ".config" / "pip"
    pip_bak = pathlib.Path.home() / ".config" / "pip.bak"
    if pip_bak.exists():
        pip_bak.rename(pip)


def configure_python():
    backup_pip()
    try:
        run("python3", "-m", "pip", "install", "--user", "pipx")
        run("python3", "-m", "pipx", "ensurepath")
        run("/home/coder/.local/bin/pipx", "install", "poetry")
        run("sudo", "npm", "install", "-g", "prettier", "pyright")
    finally:
        restore_pip()


def install_snowflake_odbc():
    with tempfile.NamedTemporaryFile() as deb_file:
        urllib.request.urlretrieve(
            "https://sfc-repo.snowflakecomputing.com/odbc/linux/3.1.0/snowflake-odbc-3.1.0.x86_64.deb",
            deb_file.name,
        )
        run("sudo", "dpkg", "-i", deb_file.name)

    subprocess.run(
        ["sudo", "sh", "-c", "cat > /etc/odbc.ini"],
        input=ODBC_INI_CONTENTS.encode("utf-8"),
        check=True,
    )
    run(
        "sudo",
        "ln",
        "-s",
        "/usr/lib/x86_64-linux-gnu/libodbcinst.so.2",
        "/usr/lib/x86_64-linux-gnu/libodbcinst.so.1",
    )


install_packages()
change_shell()
configure_git()
configure_python()
install_snowflake_odbc()
