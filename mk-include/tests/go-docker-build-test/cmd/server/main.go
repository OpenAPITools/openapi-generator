package main

import (
	"io"
	"net/http"

	log "github.com/sirupsen/logrus"
)

const (
	HelloEndpoint       = "/hello"
	HealthCheckEndpoint = "/health-check"
)

func HealthCheckHandler(w http.ResponseWriter, r *http.Request) {
	log.Info("health-check")
	w.WriteHeader(http.StatusOK)
	io.WriteString(w, "OK")
}

func HelloHandler(w http.ResponseWriter, r *http.Request) {
	log.Info("hello")
	w.WriteHeader(http.StatusOK)
	io.WriteString(w, "Hello World")
}

func newMux() (m *http.ServeMux) {
	mux := http.NewServeMux()
	mux.HandleFunc(HelloEndpoint, HelloHandler)
	mux.HandleFunc(HealthCheckEndpoint, HealthCheckHandler)
	return mux
}

func main() {
	log.Info("Starting http server")
	http.ListenAndServe("localhost:8000", newMux())
}
