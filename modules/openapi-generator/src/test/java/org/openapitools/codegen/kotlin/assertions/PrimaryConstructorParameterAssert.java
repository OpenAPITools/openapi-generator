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
public class PrimaryConstructorParameterAssert extends AbstractAssert<PrimaryConstructorParameterAssert, KtParameter> {
    private final ClassAssert classAssert;

    PrimaryConstructorParameterAssert(final ClassAssert classAssert, final KtParameter parameter) {
        super(parameter, PrimaryConstructorParameterAssert.class);
        this.classAssert = classAssert;
    }

    public PrimaryConstructorParameterAnnotationAssert assertParameterAnnotation(final String annotationName, final String useSiteTarget) {
        final List<KtAnnotationEntry> annotations = actual.getAnnotationEntries().stream()
                .filter(a ->
                        Objects.equals(a.getShortName() != null ? a.getShortName().asString() : null, annotationName)
                                && Objects.equals(a.getUseSiteTarget() != null ? a.getUseSiteTarget().getText() : null, useSiteTarget))
                .collect(Collectors.toList());
        Assertions.assertThat(annotations)
                .withFailMessage("Expected property to have a single annotation %s, but found %s", annotationName, annotations.size())
                .hasSize(1);

        return new PrimaryConstructorParameterAnnotationAssert(this, annotations.get(0));
    }

    public ClassAssert toClass() {
        return classAssert;
    }
}
