package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtParameter;
import org.jetbrains.kotlin.psi.KtSecondaryConstructor;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class SecondaryConstructorAssert extends AbstractAssert<SecondaryConstructorAssert, KtSecondaryConstructor> {

    SecondaryConstructorAssert(final KtSecondaryConstructor secondaryConstructor) {
        super(secondaryConstructor, SecondaryConstructorAssert.class);
    }

    public static SecondaryConstructorAssert assertThat(KtSecondaryConstructor secondaryConstructor) {
        return new SecondaryConstructorAssert(secondaryConstructor);
    }

    public SecondaryConstructorParameterAssert assertParameter(final String parameterName) {
        final List<KtParameter> parameters = actual.getValueParameters().stream()
                .filter(p -> Objects.equals(p.getName(), parameterName))
                .collect(Collectors.toList());
        Assertions.assertThat(parameters)
                .withFailMessage("Expected secondary constructor to have a single parameter %s, but found %s", parameterName, parameters.size())
                .hasSize(1);

        return new SecondaryConstructorParameterAssert(this, parameters.get(0));
    }
}
