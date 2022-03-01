package org.openapitools.codegen.java.assertions;

import java.util.Optional;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;

import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;

public class MethodAssert extends AbstractAssert<MethodAssert, MethodDeclaration> {

    private final JavaFileAssert fileAssert;

    MethodAssert(final JavaFileAssert fileAssert, final MethodDeclaration methodDeclaration) {
        super(methodDeclaration, MethodAssert.class);
        this.fileAssert = fileAssert;
    }

    public JavaFileAssert and() {
        return fileAssert;
    }

    public MethodAnnotationAssert assertMethodAnnotations() {
        return new MethodAnnotationAssert(this, actual.getAnnotations());
    }

    public MethodAssert hasReturnType(final String returnType) {
        Assertions.assertThat(actual.getType().toString())
            .isEqualTo(returnType);
        return this;
    }

    public ParameterAssert hasParameter(final String paramName) {
        final Optional<Parameter> parameter = actual.getParameterByName(paramName);
        Assertions.assertThat(parameter)
            .withFailMessage("Method %s should have parameter %s, but it doesn't", actual.getNameAsString(), paramName)
            .isPresent();
        return new ParameterAssert(this, parameter.get());
    }

}
