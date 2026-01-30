package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtProperty;

@CanIgnoreReturnValue
public class CompanionPropertyAssert extends AbstractAssert<CompanionPropertyAssert, KtProperty> {
    private final CompanionAssert companionAssert;

    CompanionPropertyAssert(final CompanionAssert companionAssert, final KtProperty property) {
        super(property, CompanionPropertyAssert.class);
        this.companionAssert = companionAssert;
    }

    public CompanionPropertyAssert isConst() {
        Assertions.assertThat(actual.hasModifier(org.jetbrains.kotlin.lexer.KtTokens.CONST_KEYWORD))
                .withFailMessage("Expected property %s to be const, but it was not", actual.getName())
                .isTrue();

        return this;
    }

    public CompanionPropertyAssert hasInitializer(final String expectedValue) {
        final String actualValue = actual.getInitializer() != null ? actual.getInitializer().getText() : null;
        Assertions.assertThat(actualValue)
                .withFailMessage("Expected property %s to have value %s, but was %s", actual.getName(), expectedValue, actualValue)
                .isEqualTo(expectedValue);

        return this;
    }

    public CompanionAssert toCompanion() {
        return companionAssert;
    }
}
