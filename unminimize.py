#!/bin/env python3
import os
import subprocess

os.environ["DEBIAN_FRONTEND"] = "noninteractive"


def sudo(*args, **kwargs):
    proc = subprocess.run(
        ["sudo"] + list(args), capture_output=True, encoding="utf-8", **kwargs
    )
    if proc.returncode != 0:
        print(f"Process {' '.join(proc.args)} failed with code {proc.returncode}")
        print("STDOUT:")
        print('  ' + '\n  '.join(proc.stdout.splitlines()))
        print("STDERR:")
        print('  ' + '\n  '.join(proc.stderr.splitlines()))
        raise Exception("Process failed")
    return proc.stdout.strip()

def search_packages(*patterns):
    packages = set()
    for line in sudo("dpkg", "-S", *patterns).splitlines():
        packages.update(line.split(": ")[0].split(", "))
    return packages


if any(
    os.path.exists(path)
    for path in [
        "/etc/dpkg/dpkg.cfg.d/excludes",
        "/etc/dpkg/dpkg.cfg.d/excludes.dpkg-tmp",
    ]
):
    if os.path.exists("/etc/dpkg/dpkg.cfg.d/excludes"):
        os.rename(
            "/etc/dpkg/dpkg.cfg.d/excludes",
            "/etc/dpkg/dpkg.cfg.d/excludes.dpkg-tmp",
        )
    print("Updating...")
    sudo("apt-get", "update")

    packages = set()

    print("Searching for packages that install to /usr/share/man...")
    packages.update(search_packages("/usr/share/man"))

    print("Searching for with missing files...")
    files = set()
    for line in sudo("dpkg", "--verify", "--verify-format", "rpm").splitlines():
        line = line.strip()
        if line == "":
            continue

        status, name = line.split(maxsplit=1)
        if status == 'missing' and ('/usr/share/doc' in name or '/usr/share/locale' in name):
            files.add(name)

    if len(files) > 0:
        print("Searching for packages with missing files...")
        packages.update(search_packages(*list(files)))
    else:
        print("No packages have missing files")

    # This one never works.
    packages.remove("containerd.io")

    # This one is required.
    packages.add("git-man")

    if len(packages) > 0:
        packages = list(sorted(packages))
        print(f"Fixing {len(packages)} packages...")
        sudo("apt-get", "install", "--reinstall", "-y", *packages)

if sudo("dpkg-divert", "--truename", "/usr/bin/man") == "/usr/bin/man.REAL":
    print("Removing divert for `man`...")
    sudo("rm", "-f", "/usr/bin/man")
    sudo("dpkg-divert", "--quiet", "--remove", "--rename", "/usr/bin/man")
