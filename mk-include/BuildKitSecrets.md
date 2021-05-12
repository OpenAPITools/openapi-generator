# Manage docker build secrets with BuildKit

This readme describes how to pass ssh, aws, netrc or other credentials/secrets into a docker build
using docker's BuildKit feature.

## What is BuildKit?

BuildKit is a new build backend for docker image builds. It has several improvements over the
legacy build backend in terms of performance and security.

More information can be found at:

https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information
https://blog.mobyproject.org/introducing-buildkit-17e056cc5317

## How to use BuildKit

### Add ssh keys to an ssh-agent running on the host

If you need to use ssh, the host running the docker build must have an agent with an
appropriate identity loaded and accepting connections over the default SSH_AUTH_SOCK socket.

The default `id_rsa` identity will be automatically loaded into the agent by `cc-docker.mk`. If you
use a non-default ssh identity file, manually add it using:

    ssh-add ~/.ssh/github_rsa

### Enable passing in secrets in your Makefile

Buildkit based builds are enabled by default. Enable specific default secrets you want to use in your
Dockerfile using one or more of the following make variables:

    DOCKER_SSH_MOUNT := 1
    DOCKER_NETRC_MOUNT := 1
    DOCKER_AWS_MOUNT := 1

These will enable adding the appropriate option(s) to `docker build` to pass in the corresponding
secret:

    docker build --ssh default \
            --secret id=netrc,src=/Users/${USER}/.netrc \
            --secret id=aws,src=/Users/${USER}/.aws/credentials \
            --build-arg version=ab1f195-dirty-${USER} \
            -t confluentinc/cc-docker-image-repo:ab1f195-dirty-${USER} .

While these are just the available defaults, almost any secret can be passed in by manually setting
the `DOCKER_BUILD_OPTIONS` variable with the appropriate `--secret` or `--ssh` parameter.

### Consume secrets in your Dockerfile

In your Dockerfile, add the following to the top to enable buildkit:

    # syntax=docker/dockerfile:1.0.0-experimental

Note the leading `#` is required.

For every `RUN` command that requires use of a secret, add a mount option to mount the
appropriate secret:

    RUN --mount=type=ssh git clone https://github.com/confluentinc/cc-docker-image-repo.git cc-docker-image-repo
    RUN --mount=type=ssh go get github.com/confluentinc/cc-structs
    RUN --mount=type=ssh CGO_ENABLED=0 make deps
    RUN --mount=type=secret,id=aws,target=/root/.aws/credentials aws sts get-caller-identity

The secret is mounted into a temporary filesystem that is only available for that one `RUN` command,
and never gets added to the image history. For `ssh`, the mount enables that `RUN` command to
use an SSH socket that communicates with the ssh agent running outside on the host.

Make sure to remove any of the following commands if they exist in your Dockerfile:

    COPY .ssh .ssh
    COPY .netrc ./
    COPY .aws .aws
    COPY .gitignore .gitignore

and replace

    RUN ssh-keyscan -t rsa github.com > /root/.ssh/known_hosts

with

    RUN mkdir -p -m 0600 /root/.ssh && ssh-keyscan -t rsa github.com >> /root/.ssh/known_hosts

See the test [Dockerfile](tests/go-docker-build-test/Dockerfile) and
[Makefile](tests/go-docker-build-test/Makefile) for usage examples.

## BuildKit output

BuildKit uses a tty-based progress output by default, but can be switched to plain output using
one of the following:

    # BUILDKIT_PROGRESS=plain make build-docker // applies to all BuildKit based docker builds
    # DOCKER_BUILD_OPTIONS="--progress plain" make build-docker // applies only to cc-mk-include based BuildKit docker builds

In addition, note that `docker build` output goes to stderr when using BuildKit rather than to
stdout.

## Troubleshooting

    Error response from daemon: Dockerfile parse error line 27: Unknown flag: mount

This can be due to a couple of reasons:

1. Make sure you've added the comment header in your Dockerfile. See the [Consume secrets in your
Dockerfile](#consume-secrets-in-your-dockerfile) section above.
1. Ensure your docker version is at 18.09 or higher to support buildkit.
   * Note that this also requires OS X 10.13 or higher.

## Temporary opt-out

You can temporarily opt-out of buildkit based builds by setting the following environment variable in your
Makefile:

    export DOCKER_BUILDKIT=0

Caution: this opt-out capability is temporary, and will be removed in a few weeks.
