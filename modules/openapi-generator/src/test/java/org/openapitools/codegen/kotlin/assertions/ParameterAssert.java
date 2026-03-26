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
public class ParameterAssert extends AbstractAssert<ParameterAssert, KtParameter> {
    private final MethodAssert methodAssert;

    ParameterAssert(final MethodAssert methodAssert, final KtParameter parameter) {
        super(parameter, ParameterAssert.class);
        this.methodAssert = methodAssert;
    }

    public ParameterAnnotationAssert assertParameterAnnotation(final String annotationName) {
        final List<KtAnnotationEntry> annotations = actual.getAnnotationEntries().stream()
                .filter(a -> Objects.equals(a.getShortName() != null ? a.getShortName().asString() : null, annotationName))
                .collect(Collectors.toList());
        Assertions.assertThat(annotations)
                .withFailMessage("Expected parameter to have a single annotation %s, but found %s", annotationName, annotations.size())
                .hasSize(1);

        return new ParameterAnnotationAssert(this, annotations.get(0));
    }

    public MethodAssert toMethod() {
        return methodAssert;
    }
}
