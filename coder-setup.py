import os
import pathlib
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
