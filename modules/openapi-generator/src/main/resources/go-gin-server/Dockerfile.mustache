FROM golang:1.10 AS build
WORKDIR /go/src
COPY {{apiPath}} ./{{apiPath}}
COPY main.go .

ENV CGO_ENABLED=0
RUN go get -d -v ./...

RUN go build -a -installsuffix cgo -o {{packageName}} .

FROM scratch AS runtime
ENV GIN_MODE=release
COPY --from=build /go/src/{{packageName}} ./
EXPOSE 8080/tcp
ENTRYPOINT ["./{{packageName}}"]
