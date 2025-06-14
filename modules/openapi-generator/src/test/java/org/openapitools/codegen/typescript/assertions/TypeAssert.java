package org.openapitools.codegen.typescript.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.Condition;
import org.openapitools.codegen.antlr4.TypeScriptParser;

public class TypeAssert extends AbstractAssert<TypeAssert, TypeScriptParser.Type_Context> {
    private final TypescriptFileAssert typescriptFileAssert;

    public TypeAssert(TypescriptFileAssert typescriptFileAssert, TypeScriptParser.Type_Context typeContext) {
        super(typeContext, TypeAssert.class);
        this.typescriptFileAssert = typescriptFileAssert;
    }



    public TypeAssert assertHasGeneric() {
        Condition<TypeScriptParser.Type_Context> typeHasGeneric = new Condition<>() {
            @Override
            public boolean matches(TypeScriptParser.Type_Context value) {
                return (value.unionOrIntersectionOrPrimaryType() instanceof TypeScriptParser.PrimaryContext) &&
                        ((TypeScriptParser.PrimaryContext) value.unionOrIntersectionOrPrimaryType()).primaryType() instanceof TypeScriptParser.ReferencePrimTypeContext &&
                        ((TypeScriptParser.ReferencePrimTypeContext) ((TypeScriptParser.PrimaryContext) value.unionOrIntersectionOrPrimaryType()).primaryType()).typeReference().typeGeneric() != null;
            }
        };
        Assertions.assertThat(actual)
                .withFailMessage("%s does not have generic")
                .satisfies(typeHasGeneric);
        return this;
    }

    public TypeAssert assertEquals(String type) {
        Condition<TypeScriptParser.Type_Context> typeHasGeneric = new Condition<>() {
            @Override
            public boolean matches(TypeScriptParser.Type_Context value) {
                return value.getText().equals(type);
            }
        };
        Assertions.assertThat(actual)
                .withFailMessage("Expected %s, got %s", type, actual.getText())
                .satisfies(typeHasGeneric);
        return this;
    }

    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }
}
