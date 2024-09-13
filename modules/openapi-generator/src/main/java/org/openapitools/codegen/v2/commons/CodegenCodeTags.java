package org.openapitools.codegen.v2.commons;

import org.openapitools.codegen.v2.CodegenTag;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.Collection;

public class CodegenCodeTags {
    private CodegenCodeTags() {}

    public static final CodegenTag NAME = CodegenTag.of("code.name", String.class);
    public static final CodegenTag NAMESPACE = CodegenTag.of("code.namespace", String.class);
    public static final CodegenTag TYPE = CodegenTag.of("code.type", String.class);
    public static final CodegenTag BASE_TYPE = CodegenTag.of("code.baseType", String.class);
    public static final CodegenTag RETURN_TYPE = CodegenTag.of("code.returnType", String.class);
    public static final CodegenTag GENERIC_TYPES = CodegenTag.of("code.genericTypes", new GenericClass<Collection<String>>() {});
    public static final CodegenTag INTERFACES = CodegenTag.of("code.interfaces", new GenericClass<Collection<String>>() {});
    public static final CodegenTag IMPORTS = CodegenTag.of("code.imports", new GenericClass<Collection<String>>() {});
    public static final CodegenTag ENUMS = CodegenTag.of("code.enums", new GenericClass<Collection<String>>() {});
    public static final CodegenTag ACCESSOR = CodegenTag.of("code.accessor", String.class);
    public static final CodegenTag MUTATOR = CodegenTag.of("code.mutator", String.class);
    public static final CodegenTag VISIBILITY = CodegenTag.of("code.visibility", String.class);
}
