package org.openapitools.codegen.typescript.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.antlr4.TypeScriptParser;

import java.util.Optional;

public class InterfaceAssert extends AbstractAssert<InterfaceAssert, TypeScriptParser.InterfaceDeclarationContext> {

    private final TypescriptFileAssert typescriptFileAssert;
    public InterfaceAssert(TypescriptFileAssert typescriptFileAssert, TypeScriptParser.InterfaceDeclarationContext interfaceDeclarationContext) {
        super(interfaceDeclarationContext, InterfaceAssert.class);
        this.typescriptFileAssert = typescriptFileAssert;
    }

    public PropertyAssert propertyAssert(String propertyName) {
        Optional<TypeScriptParser.PropertySignaturContext> propertySignaturContext = actual.objectType()
                .typeBody().typeMemberList().typeMember().stream().filter((tm) -> {
                    return tm.propertySignatur() != null && tm.propertySignatur().propertyName().getText().equals(propertyName);
                }).map((tm) -> tm.propertySignatur()).findFirst();
        Assertions.assertThat(propertySignaturContext)
                .withFailMessage("%s is not present or is not a property", propertyName)
                .isPresent();
        return new PropertyAssert(typescriptFileAssert, propertySignaturContext.get());
    }

    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }
}
