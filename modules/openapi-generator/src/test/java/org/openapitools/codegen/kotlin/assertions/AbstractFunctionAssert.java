package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.jetbrains.kotlin.psi.KtFunction;
import org.jetbrains.kotlin.psi.KtParameter;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public abstract class AbstractFunctionAssert<SELF extends AbstractFunctionAssert<SELF, PARAM_ASSERT>, PARAM_ASSERT> extends AbstractAssert<SELF, KtFunction> {

    AbstractFunctionAssert(final KtFunction function, final Class<SELF> selfType) {
        super(function, selfType);
    }

    abstract PARAM_ASSERT createParameterAssert(final KtParameter parameter);

    public PARAM_ASSERT assertParameter(final String parameterName) {
        final List<KtParameter> parameters = actual.getValueParameters().stream()
                .filter(p -> Objects.equals(p.getName(), parameterName))
                .collect(Collectors.toList());
        Assertions.assertThat(parameters)
                .withFailMessage("Expected function to have a single parameter %s, but found %s", parameterName, parameters.size())
                .hasSize(1);

        return createParameterAssert(parameters.get(0));
    }

    public SELF hasReturnType(final String expectedReturnType) {
        final String actualReturnType = actual.getTypeReference() != null ? actual.getTypeReference().getText() : null;
        Assertions.assertThat(actualReturnType)
                .withFailMessage("Expected function %s to have return type %s, but was %s", actual.getName(), expectedReturnType, actualReturnType)
                .isEqualTo(expectedReturnType);

        return myself;
    }
}
