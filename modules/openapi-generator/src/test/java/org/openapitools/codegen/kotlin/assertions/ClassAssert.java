package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtClass;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtParameter;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class ClassAssert extends AbstractAssert<ClassAssert, KtClass>  {
    private final KotlinFileAssert fileAssert;

    ClassAssert(final KotlinFileAssert fileAssert, final KtClass actual) {
        super(actual, ClassAssert.class);
        this.fileAssert = fileAssert;
    }

    public MethodAssert assertMethod(final String methodName) {
        Assertions.assertThat(actual.getBody())
                .withFailMessage("Expected class to have a body, but it was null")
                .isNotNull();
        final List<KtNamedFunction> methods = actual.getBody().getFunctions().stream()
                .filter(f -> Objects.equals(f.getName(), methodName))
                .collect(Collectors.toList());
        Assertions.assertThat(methods)
                .withFailMessage("Expected class to have a single method %s, but found %s", methodName, methods.size())
                .hasSize(1);

        return new MethodAssert(this, methods.get(0));
    }
    
    public PrimaryConstructorParameterAssert assertPrimaryConstructorParameter(final String propertyName) {
        final List<KtParameter> parameters = actual.getPrimaryConstructorParameters().stream()
                .filter(p -> Objects.equals(p.getName(), propertyName))
                .collect(Collectors.toList());
        Assertions.assertThat(parameters)
                .withFailMessage("Expected class to have a single property %s, but found %s", propertyName, parameters.size())
                .hasSize(1);

        return new PrimaryConstructorParameterAssert(this, parameters.get(0));
    }

    public KotlinFileAssert toFile() {
        return fileAssert;
    }
}
