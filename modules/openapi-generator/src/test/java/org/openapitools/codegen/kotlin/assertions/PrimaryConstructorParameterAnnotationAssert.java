package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class PrimaryConstructorParameterAnnotationAssert extends AbstractAnnotationAssert<PrimaryConstructorParameterAnnotationAssert> {
    private final PrimaryConstructorParameterAssert parameterAssert;

    PrimaryConstructorParameterAnnotationAssert(final PrimaryConstructorParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, PrimaryConstructorParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public PrimaryConstructorParameterAssert toPrimaryConstructorParameter() {
        return parameterAssert;
    }
}
