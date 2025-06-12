package org.openapitools.codegen.typescript.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.Condition;
import org.openapitools.codegen.antlr4.TypeScriptParser;

public class PropertyAssert extends AbstractAssert<PropertyAssert, TypeScriptParser.PropertySignaturContext> {

    private final TypescriptFileAssert typescriptFileAssert;

    public PropertyAssert(TypescriptFileAssert typescriptFileAssert, TypeScriptParser.PropertySignaturContext propertySignaturContext) {
        super(propertySignaturContext, PropertyAssert.class);
        this.typescriptFileAssert = typescriptFileAssert;
    }

    public PropertyAssert isGeneric() {
        Condition<TypeScriptParser.PropertySignaturContext> hasType = new Condition<>() {
            @Override
            public boolean matches(TypeScriptParser.PropertySignaturContext value) {
                return value.typeAnnotation() != null;
            }
        };
        Condition<TypeScriptParser.TypeAnnotationContext> typeGeneric = new Condition<>() {
            @Override
            public boolean matches(TypeScriptParser.TypeAnnotationContext value) {
                return value.type_().typeGeneric() != null;
            }
        };
        Assertions.assertThat(actual)
                .withFailMessage("Type is not specified")
                .satisfies(hasType);
        Assertions.assertThat(actual.typeAnnotation())
                .withFailMessage("Type is not generic")
                .satisfies(typeGeneric);
        return this;
    }

    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }
}
