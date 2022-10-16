module github.com/OpenAPITools/openapi-generator/samples/openapi3/client/petstore/go

go 1.16

replace go-petstore => ./go-petstore

require (
	github.com/stretchr/testify v1.8.0
	go-petstore v0.0.0-00010101000000-000000000000
	golang.org/x/net v0.0.0-20221004154528-8021a29435af // indirect
	golang.org/x/oauth2 v0.0.0-20221006150949-b44042a4b9c1
)
