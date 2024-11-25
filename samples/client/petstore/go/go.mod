module github.com/OpenAPITools/openapi-generator/samples/client/petstore/go

replace github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore => ./go-petstore

go 1.13

require (
	cloud.google.com/go/compute v1.20.1 // indirect
	github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore v0.0.0-00010101000000-000000000000
	github.com/go-openapi/swag v0.22.3
	github.com/stretchr/testify v1.9.0
	golang.org/x/net v0.27.0 // indirect
	golang.org/x/oauth2 v0.21.0
	google.golang.org/protobuf v1.31.0 // indirect
)
