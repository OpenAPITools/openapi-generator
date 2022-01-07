FROM buildpack-deps:bionic
COPY setup.sh /
RUN bash setup.sh && rm /setup.sh
RUN apt-get update && \
    apt-get install -y python-pip python-setuptools && \
    rm -rf /var/cache/apt/lists