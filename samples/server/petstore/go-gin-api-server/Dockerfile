FROM golang:1.19 AS build
WORKDIR /go/src
COPY go ./go
COPY main.go .
COPY go.mod .
COPY go.sum .

ENV CGO_ENABLED=0
RUN go install

RUN go build -a -installsuffix cgo -o petstoreserver .

FROM scratch AS runtime
ENV GIN_MODE=release
COPY --from=build /go/src/petstoreserver ./
EXPOSE 8080/tcp
ENTRYPOINT ["./petstoreserver"]
