"""
go docker build process:
- make init-ci
- make build-docker
- make test
- make release-ci
"""
import time
from tempfile import TemporaryDirectory
import pytest

import structlog
structlog.configure(logger_factory=structlog.stdlib.LoggerFactory())

from tests.test_utils import *


def test_version():
    assert_version()

def test_make_show_args():
    output = run_cmd("make show-args")
    assert_in_output(output, ["docker-login-ci install-vault vault-bash-functions deps cache-docker-base-images gcloud-install cpd-update helm-setup-ci",
                            "CI_BIN:               /home/semaphore/go/src/github.com/confluentinc/cc-mk-include/ci-bin"])

def test_make_init_ci():
    output = run_cmd("make init-ci")
    assert_in_output(output, ["all modules verified",
                    "cache restore confluent-docker.jfrog.io/confluentinc/cc-built-base:v1.1.0",
                    "## Updating CPD binary to latest"])
    assert_file(["/home/semaphore/.docker/config.json"])

def test_make_build_docker():
    output = run_cmd("make build-docker 2>&1")
    assert_in_output(output, ["naming to",
                    "docker image save"])

def test_make_test():
    output = run_cmd("make test")
    assert_in_output(output, ["0 chart(s) failed"])

def test_make_release():
    output = run_cmd("make release-ci")
    assert_not_in_output(output, ["Changes not staged for commit:",
                                "recipe for target 'pre-release-check' failed"])

    assert_in_output(output, ["git add release.svg",
                    "docker push confluent-docker.jfrog.io/confluentinc/cc-test-service:latest"])

def test_make_ssh():
    env = os.environ.copy()
    # Explicitly disable DOCKER_BUILDKIT mode so we can test the ssh targets
    env["DOCKER_BUILDKIT"] = "0"
    with TemporaryDirectory() as home:
        env["HOME"] = home
        os.mkdir(os.path.join(home, ".ssh"))

        for filename in ["a", "b", "c"]:
            # Allow the modified timestamp to elapse so Make can detect change
            time.sleep(1)
            with open(os.path.join(home, ".ssh", filename), "w+") as file:
                file.write("contents")
            output = run_cmd("make .ssh", env=env)
            assert_not_in_output(output, ["up to date", "Nothing to be done"])
            assert_file([os.path.join(".ssh", filename)])
