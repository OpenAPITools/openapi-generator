package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
abstract class AbstractAnnotationAssert<SELF extends AbstractAnnotationAssert<SELF>> extends AbstractAssert<SELF, KtAnnotationEntry> {

    AbstractAnnotationAssert(final KtAnnotationEntry annotationEntry, final Class<?> selfType) {
        super(annotationEntry, selfType);
    }

    public SELF hasAttributes(final Map<String, String> expectedAttributes) {
        final Map<String, String> actualAttributes = actual.getValueArguments().stream()
                .collect(Collectors.toMap(
                        a -> a.getArgumentName() != null ? a.getArgumentName().getAsName().asString() : null,
                        a -> a.getArgumentExpression() != null ? a.getArgumentExpression().getText() : null
                ));
        final boolean allAttributesFound = expectedAttributes.entrySet().stream()
                .allMatch(expected -> Objects.equals(actualAttributes.get(expected.getKey()), expected.getValue()));
        Assertions.assertThat(allAttributesFound)
                .withFailMessage("Expected annotation to have attributes %s, but has %s", expectedAttributes, actualAttributes)
                .isTrue();

        return myself;
    }

    public SELF hasNotAttributes(final List<String> notExpectedAttributes) {
        final List<String> actualAttributes = actual.getValueArguments().stream()
                .map(a -> a.getArgumentName() != null ? a.getArgumentName().getAsName().asString() : null)
                .collect(Collectors.toList());
        Assertions.assertThat(actualAttributes)
                .withFailMessage("Expected annotation to not have attributes %s, but has %s", notExpectedAttributes, actualAttributes)
                .noneMatch(notExpectedAttributes::contains);

        return myself;
    }
}
