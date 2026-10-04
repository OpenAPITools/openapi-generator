package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class MethodParameterAnnotationAssert extends AbstractAnnotationAssert<MethodParameterAnnotationAssert> {
    private final MethodParameterAssert parameterAssert;

    MethodParameterAnnotationAssert(final MethodParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, MethodParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public MethodParameterAssert toParameter() {
        return parameterAssert;
    }
}
