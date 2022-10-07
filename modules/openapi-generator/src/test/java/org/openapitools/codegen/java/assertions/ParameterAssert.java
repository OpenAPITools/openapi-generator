package org.openapitools.codegen.java.assertions;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.ObjectAssert;
import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.ast.body.Parameter;

@CanIgnoreReturnValue
public class ParameterAssert extends ObjectAssert<Parameter> {

    private final MethodAssert methodAssert;
    private final ConstructorAssert constructorAssert;

    protected ParameterAssert(final MethodAssert methodAssert, final Parameter parameter) {
        super(parameter);
        this.methodAssert = methodAssert;
        this.constructorAssert = null;
    }

    protected ParameterAssert(final ConstructorAssert constructorAssert, final Parameter parameter) {
        super(parameter);
        this.constructorAssert = constructorAssert;
        this.methodAssert = null;
    }

    public MethodAssert toMethod() {
        if (methodAssert == null) {
            throw new IllegalArgumentException("No method assert for constructor's parameter");
        }
        return methodAssert;
    }

    public ConstructorAssert toConstructor() {
        if (constructorAssert == null) {
            throw new IllegalArgumentException("No constructor assert for method's parameter");
        }
        return constructorAssert;
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
