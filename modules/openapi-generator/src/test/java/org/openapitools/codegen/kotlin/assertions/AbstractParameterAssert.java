package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;
import org.jetbrains.kotlin.psi.KtParameter;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
abstract class AbstractParameterAssert<SELF extends AbstractParameterAssert<SELF, ANNOTATION_ASSERT>, ANNOTATION_ASSERT> extends AbstractAssert<SELF, KtParameter> {

    AbstractParameterAssert(final KtParameter parameter, final Class<?> selfType) {
        super(parameter, selfType);
    }

    abstract ANNOTATION_ASSERT createAnnotationAssert(final KtAnnotationEntry annotation);

    public ANNOTATION_ASSERT assertParameterAnnotation(final String annotationName) {
        final List<KtAnnotationEntry> annotations = actual.getAnnotationEntries().stream()
                .filter(a -> Objects.equals(a.getShortName() != null ? a.getShortName().asString() : null, annotationName))
                .collect(Collectors.toList());
        Assertions.assertThat(annotations)
                .withFailMessage("Expected parameter to have a single annotation %s, but found %s", annotationName, annotations.size())
                .hasSize(1);

        return createAnnotationAssert(annotations.get(0));
    }

    public SELF hasType(final String expectedType) {
        final String actualType = actual.getTypeReference() != null ? actual.getTypeReference().getText() : null;
        Assertions.assertThat(actualType)
                .withFailMessage("Expected parameter %s to have type %s, but was %s", actual.getName(), expectedType, actualType)
                .isEqualTo(expectedType);

        return myself;
    }

    public SELF hasDefaultValue(final String expectedValue) {
        final String actualValue = actual.getDefaultValue() != null ? actual.getDefaultValue().getText() : null;
        Assertions.assertThat(actualValue)
                .withFailMessage("Expected parameter %s to have default value %s, but was %s", actual.getName(), expectedValue, actualValue)
                .isEqualTo(expectedValue);

        return myself;
    }
}
