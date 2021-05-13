'''
maven docker build process:
- make init-ci
- make build
test mvn-docker-package
test BUILD_DOCKER_OVERRIDE
- make test
- make release-ci
- make epilogue-ci
'''
import pytest

import structlog
structlog.configure(logger_factory=structlog.stdlib.LoggerFactory())

from tests.test_utils import *


def test_version():
    assert_version()

def test_make_show_args():
    output = run_cmd("make show-args")
    assert_in_output(output, ["docker-login-ci install-vault vault-bash-functions cache-docker-base-images gcloud-install cpd-update ci-bin-sem-cache-restore helm-setup-ci",
                            "CI_BIN:               /home/semaphore/go/src/github.com/confluentinc/cc-mk-include/ci-bin"])

def test_make_init_ci():
    output = run_cmd("make init-ci")
    assert_in_output(output, ["cache restore confluent-docker.jfrog.io/confluentinc/cc-base:v6.2.0",
                    "## Updating CPD binary to latest"])
    assert_file(["/home/semaphore/.docker/config.json"])

def test_make_build():
    output = run_cmd("make build")
    assert_in_output(output, ["mvnw",
                    "BUILD SUCCESS",
                    "docker image save"])

def test_build_docker_override_one():
    output = run_cmd("make -f Makefile_test_mvn_docker_package build")
    assert_not_in_output(output, ["mvnw"])
    assert_in_output(output,["BUILD SUCCESS",
                    "docker image save"
    ])

def test_build_docker_override_two():
    output = run_cmd("make -f Makefile_test_BUILD_DOCKER_OVERRIDE build")
    assert_not_in_output(output, ["mvnw"])
    assert_in_output(output,["mvn --no-transfer-progress --batch-mode",
                    "BUILD SUCCESS",
                    "docker image save"
    ])

def test_make_test():
    output = run_cmd("make test")
    assert_in_output(output,["T E S T S",
                    "BUILD SUCCESS"
    ])

def test_make_release():
    output = run_cmd("make release-ci")
    assert_not_in_output(output, ["Changes not staged for commit:",
                                "recipe for target 'pre-release-check' failed"])
    assert_in_output(output, ["git add --verbose pom.xml '*/pom.xml'",
                            "git add release.svg",
                            "docker push confluent-docker.jfrog.io/confluentinc/cc-test-service:latest"])

def test_make_epilogue_ci():
    run_cmd("make epilogue-ci")
