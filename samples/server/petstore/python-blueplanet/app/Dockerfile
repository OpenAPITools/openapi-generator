FROM python:3-alpine

ARG GLTOKEN

RUN apk add curl bash
ENV SHELL /bin/bash

RUN mkdir /bp2 \
 && mkdir /bp2/data \
 && mkdir /bp2/log \
 && mkdir /bp2/src \
 && echo 'alias ll="ls -l"' >> ~/.bashrc
WORKDIR /bp2/src

COPY requirements.txt /bp2/src

RUN pip3 install --extra-index-url https://GLTOKEN:$GLTOKEN@pypi.blueplanet.com/simple --no-cache-dir -r requirements.txt

COPY . /bp2/src

ENV SBIS=bpocore \
    SBI_bpocore_southbound-update=update_etc_hosts_multi_provider

EXPOSE 8080

ENTRYPOINT ["python3"]

CMD ["-B", "-m", "openapi_server"]
