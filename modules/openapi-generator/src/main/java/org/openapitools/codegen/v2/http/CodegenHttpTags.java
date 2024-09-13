package org.openapitools.codegen.v2.http;

import org.openapitools.codegen.v2.CodegenTag;

public class CodegenHttpTags {
    private CodegenHttpTags() {}

    public static final CodegenTag URL = CodegenTag.of("http.url", String.class);
    public static final CodegenTag ROUTE = CodegenTag.of("http.route", String.class);
    public static final CodegenTag METHOD = CodegenTag.of("http.method", String.class);
    public static final CodegenTag CONSUMES = CodegenTag.of("http.consumes", String.class);
    public static final CodegenTag PRODUCES = CodegenTag.of("http.produces", String.class);
}
