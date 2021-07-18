package org.openapitools.codegen.v2;

import io.swagger.v3.oas.models.OpenAPI;

public interface Codegen {
    void generate(OpenAPI openAPI, CodegenOptions options);
}
