FROM debian:stretch-slim AS build

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        cmake \
        git \
        build-essential \
        libssl-dev \        
        qtbase5-dev \       
        qtbase5-dev-tools \
        ca-certificates 
RUN rm -rf /var/lib/apt/lists/*
 
WORKDIR /usr/server
ADD ./src ./src
ADD ./CMakeLists.txt ./
RUN mkdir -p ./build
WORKDIR /usr/server/build
RUN cmake ..
RUN make 

FROM debian:stretch-slim AS runtime
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libc6 \
        libstdc++6  \
        libqt5core5a \
        libqt5network5 \
        libssl1.1

RUN rm -rf /var/lib/apt/lists/*
WORKDIR /usr/server
COPY --from=build /usr/server/build/src/cpp-qt5-server ./build/src/
COPY --from=build /usr/server/external/ ./external
ENTRYPOINT ["/usr/server/build/src/cpp-qt5-server"]