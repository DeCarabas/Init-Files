"""debug_ephemeral.py - attach to privileged ephemeral containers in k8s

Usage:
  debug_ephemeral <namespace> <pod> [<target> [<image>]]

Where:
  <namespace> is the namespace of the pod. Use "default" or "-" for the default
              namespace.

        <pod> is the name of the pod to debug.

     <target> is the name of the container to attach to. If ommitted or "-", the
              ephemeral container uses the kubectl.kubernetes.io/default-container
              annotation for selecting the container, or the first container in
              the pod will be chosen.

      <image> is the name of the image to use for the ephemeral container. If
              missing, the ephemeral container uses the "ubuntu" image.
"""
import subprocess
import json
import random
import sys
import time


def slug():
    return "".join(random.choices("bcdfghjklmnpqrstvwxz2456789", k=5))


def get_default_target(namespace, pod):
    pod = json.loads(
        subprocess.run(
            ["kubectl", "get", "pod", "--namespace", namespace, pod, "-o", "json"],
            check=True,
            capture_output=True,
            encoding="utf-8",
        ).stdout
    )

    annotation_default = (
        pod.get("metadata", {})
        .get("annotations", {})
        .get("kubectl.kubernetes.io/default-container")
    )
    if annotation_default is not None:
        return annotation_default

    return pod["spec"]["containers"][0]["name"]


def get_pod_container_state(namespace, pod, container):
    pod = json.loads(
        subprocess.run(
            ["kubectl", "get", "pod", "--namespace", namespace, pod, "-o", "json"],
            check=True,
            capture_output=True,
            encoding="utf-8",
        ).stdout
    )

    statuses = pod.get("status", {}).get("ephemeralContainerStatuses", [])
    for status in statuses:
        if status.get("name", None) != container:
            state_keys = list(status.get("state", {"waiting": {}}).keys())
            if len(state_keys) == 0:
                return "waiting"
            if len(state_keys) > 1:
                return "INTERNAL ERROR"
            return state_keys[0]

    return None


def create_debugger_container(namespace, pod, target, image):
    # kubectl debug doesn't know how to do this: create an ephemeral pod with
    # a better security context. But you can do it, you just need to sent the
    # PATCH to the controller directly. Start up a `kubectl proxy` to handle
    # the traffic.
    proxy = subprocess.Popen(["kubectl", "proxy", "--port=0"], stdout=subprocess.PIPE)
    try:
        # We asked for a random port
        service_line = proxy.stdout.readline().decode("utf-8").strip()
        PREFIX = "Starting to serve on 127.0.0.1:"
        if not service_line.startswith(PREFIX):
            raise Exception("Cannot get the port from the kubectl proxy")
        port = service_line[len(PREFIX) :]

        # Pod must exist, yay!
        container_name = f"debugger-{slug()}"
        patch = json.dumps(
            {
                "spec": {
                    "ephemeralContainers": [
                        {
                            "image": image,
                            "name": container_name,
                            "resources": {},
                            "stdin": True,
                            "targetContainerName": target,
                            "terminationMessagePolicy": "File",
                            "tty": True,
                            "securityContext": {"privileged": True},
                        }
                    ]
                }
            }
        )

        curl = subprocess.run(
            [
                "curl",
                "-v",
                "-XPATCH",
                "-H",
                "Content-Type: application/strategic-merge-patch+json",
                "-H",
                "Accept: application/json, */*",
                "-H",
                "User-Agent: kubectl/v1.26.1 (linux/amd64) kubernetes/8f94681",
                f"http://127.0.0.1:{port}/api/v1/namespaces/{namespace}/pods/{pod}/ephemeralcontainers",
                "--data-binary",
                "@-",
            ],
            input=patch,
            encoding="utf-8",
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if curl.returncode != 0:
            raise Exception(f"curl failed with code {curl.returncode}:\n{curl.stdout}")

        return container_name

    finally:
        proxy.terminate()
        proxy.wait()


def attach_debugger(namespace, pod, target, image):
    if namespace == "-":
        namespace = "default"
    if image is None:
        image = "ubuntu"
    if target is None or target == "-":
        target = get_default_target(namespace, pod)

    print(f"Creating ephemeral debugging container attached to '{target}' in '{pod}'")
    container = create_debugger_container(namespace, pod, target, image)
    print(f"Created container {container}")

    # Wait for the dang container to be ready.
    for i in range(3000):
        state = get_pod_container_state(namespace, pod, container)
        if state == "running":
            break
        time.sleep(0.100)  # 100ms
    else:
        raise Exception("Timeout waiting for container to become running")

    # Container is ready, attach
    print(f"Attaching to {container}...")
    subprocess.run(
        ["kubectl", "attach", "-it", "--namespace", namespace, pod, "-c", container],
        check=True,
    )


if __name__ == "__main__":
    args = sys.argv
    if len(args) < 3:
        print(__doc__)
        sys.exit(-1)

    namespace = args[1]
    pod = args[2]
    if len(args) >= 4:
        target = args[3]
    else:
        target = None
    if len(args) >= 5:
        image = args[4]
    else:
        image = None

    attach_debugger(namespace, pod, target, image)
