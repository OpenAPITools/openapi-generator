package org.openapitools.codegen.v2.templates;

import org.openapitools.codegen.v2.CodegenTag;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.Collection;

public final class CodegenTemplateTags {
    private CodegenTemplateTags() { }

    public static final CodegenTag TEMPLATES = CodegenTag.of("template.templates", new GenericClass<Collection<CodegenTemplate>>() {});
}