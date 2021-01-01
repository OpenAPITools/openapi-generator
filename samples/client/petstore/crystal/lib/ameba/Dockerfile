FROM alpine:3.12 as builder
RUN apk add --update crystal shards openssl-dev yaml-dev musl-dev make
RUN mkdir /ameba
WORKDIR /ameba
COPY . /ameba/
RUN make clean && make

FROM alpine:3.12
RUN apk add --update openssl yaml pcre gc libevent libgcc
RUN mkdir /src
WORKDIR /src
COPY --from=builder /ameba/bin/ameba /usr/bin/
ENTRYPOINT [ "/usr/bin/ameba" ]
