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

    public TypeAssert propertyTypeAssert() {
        Assertions.assertThat(actual.typeAnnotation())
                .withFailMessage("%s does not have type", actual.propertyName().getText())
                .isNotNull();
        return new TypeAssert(typescriptFileAssert, actual.typeAnnotation().type_());
    }

    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }
}
