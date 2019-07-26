load("//:defs.bzl", "openapi_generator")

openapi_generator(
    name = "petstore",
    generator = "go",
    spec = "petstore.yaml",
)

openapi_generator(
    name = "petstore_java",
    generator = "java",
    spec = "petstore.yaml",
)

openapi_generator(
    name = "petstore_java_feign",
    additional_properties = {
        "library": "feign",
    },
    generator = "java",
    spec = "petstore.yaml",
)

openapi_generator(
    name = "petstore_java_no_tests",
    generator = "java",
    spec = "petstore.yaml",
    system_properties = {
        "apiTests": "false",
        "modelTests": "false",
    },
)

openapi_generator(
    name = "petstore_java_bigdecimal",
    generator = "java",
    spec = "petstore.yaml",
    type_mappings = {
        "Integer": "java.math.BigDecimal",
    },
)
