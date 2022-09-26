module github.com/OpenAPITools/openapi-generator/samples/openapi3/client/petstore/go

go 1.16

replace go-petstore => ./go-petstore

require (
	github.com/stretchr/testify v1.8.0
	go-petstore v0.0.0-00010101000000-000000000000
	golang.org/x/net v0.0.0-20220809184613-07c6da5e1ced // indirect
	golang.org/x/oauth2 v0.0.0-20220808172628-8227340efae7
)
