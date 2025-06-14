package org.openapitools.codegen.typescript.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.antlr4.TypeScriptParser;

public class MethodDeclarationAssert extends AbstractAssert<MethodDeclarationAssert, TypeScriptParser.MethodDeclarationExpressionContext> {

    private final TypescriptFileAssert typescriptFileAssert;
    public MethodDeclarationAssert(TypescriptFileAssert typescriptFileAssert, TypeScriptParser.MethodDeclarationExpressionContext methodDeclarationExpressionContext) {
        super(methodDeclarationExpressionContext, MethodDeclarationAssert.class);
        this.typescriptFileAssert = typescriptFileAssert;
    }

    public TypeAssert returnTypeAssert() {
        TypeScriptParser.TypeAnnotationContext returnType = actual.callSignature().typeAnnotation();
        Assertions.assertThat(returnType)
                .withFailMessage("%s does not have a return type", actual.propertyName())
                .isNotNull();
        return new TypeAssert(typescriptFileAssert, returnType.type_());
    }


    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }

}
