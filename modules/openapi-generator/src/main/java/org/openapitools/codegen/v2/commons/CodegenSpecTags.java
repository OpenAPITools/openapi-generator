package org.openapitools.codegen.v2.commons;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.v2.CodegenTag;

public class CodegenSpecTags {
    private CodegenSpecTags() {}
    public static final CodegenTag AVAILABLE = CodegenTag.of("spec.available", Boolean.class);
    public static final CodegenTag OPEN_API = CodegenTag.of("spec.openAPI", OpenAPI.class);
    public static final CodegenTag SCHEMA = CodegenTag.of("spec.schema", Schema.class);
    public static final CodegenTag OPERATION = CodegenTag.of("spec.operation", Schema.class);
    public static final CodegenTag PARAMETER = CodegenTag.of("spec.parameter", Schema.class);
    public static final CodegenTag PROPERTY = CodegenTag.of("spec.property", Schema.class);
}
