FROM alpine:latest AS build

RUN apk add --update \
        cmake \
        alpine-sdk \
        openssl \
        qt5-qtbase-dev \
        qt5-qttools-dev

WORKDIR /usr/server
ADD ./src ./src
ADD ./CMakeLists.txt ./
RUN mkdir -p ./build
WORKDIR /usr/server/build
RUN cmake  -DNODEBUG:STRING="ON" ..
RUN make

FROM alpine:latest AS runtime
RUN apk add --update \
        libgcc  \
        libstdc++  \
        qt5-qtbase \
        openssl

WORKDIR /usr/server
COPY --from=build /usr/server/build/src/cpp-qt5-qhttpengine-server ./build/src/
COPY --from=build /usr/server/external/ ./external
EXPOSE 8080/tcp
ENTRYPOINT ["/usr/server/build/src/cpp-qt5-qhttpengine-server"]