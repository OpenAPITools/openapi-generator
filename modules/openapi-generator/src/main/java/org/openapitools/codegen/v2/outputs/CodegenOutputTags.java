package org.openapitools.codegen.v2.outputs;

import org.openapitools.codegen.v2.CodegenTag;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.Collection;

public final class CodegenOutputTags {
    private CodegenOutputTags() { }

    public static final CodegenTag OUTPUTS = CodegenTag.of("output.outputs", new GenericClass<Collection<CodegenOutput>>(){});
}