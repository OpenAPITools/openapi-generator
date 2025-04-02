module github.com/OpenAPITools/openapi-generator/samples/openapi3/client/petstore/go

go 1.16

replace go-petstore => ./go-petstore

require (
	cloud.google.com/go/compute v1.20.1 // indirect
	github.com/stretchr/testify v1.10.0
	go-petstore v0.0.0-00010101000000-000000000000
	golang.org/x/net v0.31.0 // indirect
	golang.org/x/oauth2 v0.24.0
	google.golang.org/protobuf v1.31.0 // indirect
	gopkg.in/validator.v2 v2.0.1 // indirect
)
