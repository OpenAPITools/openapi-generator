FROM golang:1.19 AS build
WORKDIR /go/src
COPY go ./go
COPY main.go .
COPY go.sum .
COPY go.mod .

ENV CGO_ENABLED=0

RUN go build -o petstoreserver .

FROM scratch AS runtime
COPY --from=build /go/src/petstoreserver ./
EXPOSE 8080/tcp
ENTRYPOINT ["./petstoreserver"]
