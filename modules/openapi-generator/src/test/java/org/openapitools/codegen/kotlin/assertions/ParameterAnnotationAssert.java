package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class ParameterAnnotationAssert extends AbstractAnnotationAssert<ParameterAnnotationAssert> {
    private final ParameterAssert parameterAssert;

    ParameterAnnotationAssert(final ParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, ParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public ParameterAssert toParameter() {
        return parameterAssert;
    }
}
