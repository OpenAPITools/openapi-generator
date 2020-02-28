package org.openapitools.codegen.v2.commons;

import org.openapitools.codegen.v2.CodegenTag;
import org.openapitools.codegen.v2.reflection.GenericClass;

public class CodegenDocTags {
    private CodegenDocTags() {}
    public static final CodegenTag AVAILABLE = CodegenTag.of("doc.available", Boolean.class);
    public static final CodegenTag SUMMARY = CodegenTag.of("doc.summary", String.class);
    public static final CodegenTag NOTE = CodegenTag.of("doc.note", String.class);
}
