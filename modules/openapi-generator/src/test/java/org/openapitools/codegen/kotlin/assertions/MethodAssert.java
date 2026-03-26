package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtParameter;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class MethodAssert extends AbstractAssert<MethodAssert, KtNamedFunction> {
    private final ClassAssert classAssert;

    MethodAssert(final ClassAssert classAssert, final KtNamedFunction method) {
        super(method, MethodAssert.class);
        this.classAssert = classAssert;
    }

    public ParameterAssert assertParameter(final String parameterName) {
        final List<KtParameter> parameters = actual.getValueParameters().stream()
                .filter(p -> Objects.equals(p.getName(), parameterName))
                .collect(Collectors.toList());
        Assertions.assertThat(parameters)
                .withFailMessage("Expected class to have a single parameter %s, but found %s", parameterName, parameters.size())
                .hasSize(1);

        return new ParameterAssert(this, parameters.get(0));
    }

    public ClassAssert toClass() {
        return classAssert;
    }
}
