# syntax=docker/dockerfile:1.0.0-experimental
FROM golang:1.14.2 AS builder

WORKDIR /root
RUN mkdir -p -m 0600 .ssh && ssh-keyscan -t rsa github.com >> .ssh/known_hosts

# Verify keys are loaded into ssh-agent on docker host
RUN --mount=type=ssh ssh -T git@github.com 2>&1 | grep -v "Permission denied"

WORKDIR /root/cc-test-service

COPY go.mod go.sum Makefile ./
COPY mk-include ./mk-include
RUN --mount=type=ssh CGO_ENABLED=0 make deps

COPY . .

ARG version
RUN CGO_ENABLED=0 make build-go GO_OUTDIR= VERSION=${version}

FROM confluent-docker.jfrog.io/confluentinc/cc-built-base:v1.1.0

COPY --from=0 /test-service /

ENTRYPOINT ["/test-service"]
