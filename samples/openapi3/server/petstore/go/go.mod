module github.com/OpenAPITools/openapi-generator/samples/openapi3/server/petstore/go

go 1.18

require (
	github.com/stretchr/testify v1.8.4
	go-petstore v0.0.0-00010101000000-000000000000
)

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/go-chi/chi/v5 v5.0.8 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

replace go-petstore => ./go-petstore
