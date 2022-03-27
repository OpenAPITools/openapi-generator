package org.openapitools.codegen.java.assertions;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.ObjectAssert;

import com.github.javaparser.ast.body.Parameter;

public class ParameterAssert extends ObjectAssert<Parameter> {

    private final MethodAssert methodAssert;

    protected ParameterAssert(final MethodAssert methodAssert, final Parameter parameter) {
        super(parameter);
        this.methodAssert = methodAssert;
    }

    public MethodAssert toMethod() {
        return methodAssert;
    }

    public ParameterAssert withType(final String expectedType) {
        Assertions.assertThat(actual.getTypeAsString())
            .withFailMessage("Expected parameter to have type %s, but was %s", expectedType, actual.getTypeAsString())
            .isEqualTo(expectedType);
        return this;
    }

    public ParameterAnnotationAssert assertParameterAnnotations() {
        return new ParameterAnnotationAssert(this, actual.getAnnotations());
    }
}
