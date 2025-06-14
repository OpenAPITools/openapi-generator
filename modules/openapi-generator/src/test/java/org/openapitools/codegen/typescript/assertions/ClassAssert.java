package org.openapitools.codegen.typescript.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.antlr4.TypeScriptParser;

import java.util.Optional;

public class ClassAssert extends AbstractAssert<ClassAssert, TypeScriptParser.ClassDeclarationContext> {

    private final TypescriptFileAssert typescriptFileAssert;
    public ClassAssert(TypescriptFileAssert typescriptFileAssert, TypeScriptParser.ClassDeclarationContext classDeclarationContext) {
        super(classDeclarationContext, ClassAssert.class);
        this.typescriptFileAssert = typescriptFileAssert;
    }

    public MethodDeclarationAssert methodDeclarationAssert(String methodName) {
        Optional<TypeScriptParser.MethodDeclarationExpressionContext> methodDeclarationExpressionContext = actual.classTail().classElement()
                .stream().filter((ce) -> {
                    return ce.propertyMemberDeclaration() instanceof TypeScriptParser.MethodDeclarationExpressionContext &&
                            ((TypeScriptParser.MethodDeclarationExpressionContext) ce.propertyMemberDeclaration()).propertyName().getText().equals(methodName);
                }).map((ce) -> (TypeScriptParser.MethodDeclarationExpressionContext) ce.propertyMemberDeclaration()).findFirst();
        Assertions.assertThat(methodDeclarationExpressionContext)
                .withFailMessage("%s is not present or is not a method", methodName)
                .isPresent();
        return new MethodDeclarationAssert(typescriptFileAssert, methodDeclarationExpressionContext.get());
    }

    public TypescriptFileAssert toFileAssert() {
        return typescriptFileAssert;
    }
}
