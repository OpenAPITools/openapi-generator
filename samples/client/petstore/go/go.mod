module github.com/OpenAPITools/openapi-generator/samples/client/petstore/go

replace github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore => ./go-petstore

go 1.13

require (
	github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore v0.0.0-00010101000000-000000000000
	github.com/google/go-cmp v0.6.0 // indirect
	github.com/kr/pretty v0.3.0 // indirect
	github.com/rogpeppe/go-internal v1.9.0 // indirect
	github.com/stretchr/testify v1.9.0
	golang.org/x/oauth2 v0.21.0
	gopkg.in/check.v1 v1.0.0-20201130134442-10cb98267c6c // indirect
)
