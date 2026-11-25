package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtObjectDeclaration;
import org.jetbrains.kotlin.psi.KtProperty;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class CompanionAssert extends AbstractAssert<CompanionAssert, KtObjectDeclaration> {
    private final ClassAssert classAssert;

    CompanionAssert(final ClassAssert classAssert, final KtObjectDeclaration companion) {
        super(companion, CompanionAssert.class);
        this.classAssert = classAssert;
    }

    public CompanionPropertyAssert assertProperty(final String propertyName) {
        Assertions.assertThat(actual.getBody())
                .withFailMessage("Expected companion to have a body, but it was null")
                .isNotNull();
        final List<KtProperty> properties = actual.getBody().getProperties().stream()
                .filter(p -> Objects.equals(p.getName(), propertyName))
                .collect(Collectors.toList());
        Assertions.assertThat(properties)
                .withFailMessage("Expected companion object to have a single property %s, but found %s", propertyName, properties.size())
                .hasSize(1);

        return new CompanionPropertyAssert(this, properties.get(0));
    }

    public CompanionFunctionAssert assertFunction(final String functionName) {
        Assertions.assertThat(actual.getBody())
                .withFailMessage("Expected companion to have a body, but it was null")
                .isNotNull();
        final List<KtNamedFunction> functions = actual.getBody().getFunctions().stream()
                .filter(f -> Objects.equals(f.getName(), functionName))
                .collect(Collectors.toList());
        Assertions.assertThat(functions)
                .withFailMessage("Expected companion object to have a single function %s, but found %s", functionName, functions.size())
                .hasSize(1);

        return new CompanionFunctionAssert(classAssert, functions.get(0));
    }

    public ClassAssert toClass() {
        return classAssert;
    }
}
