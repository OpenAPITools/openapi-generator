package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class MethodAnnotationAssert extends AbstractAnnotationAssert<MethodAnnotationAssert> {
    private final MethodAssert methodAssert;

    MethodAnnotationAssert(final MethodAssert methodAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, MethodAnnotationAssert.class);
        this.methodAssert = methodAssert;
    }

    public MethodAssert toMethod() {
        return methodAssert;
    }
}
