package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class MethodParameterAssert extends AbstractParameterAssert<MethodParameterAssert, MethodParameterAnnotationAssert> {
    private final MethodAssert methodAssert;

    MethodParameterAssert(final MethodAssert methodAssert, final KtParameter parameter) {
        super(parameter, MethodParameterAssert.class);
        this.methodAssert = methodAssert;
    }

    MethodParameterAnnotationAssert createAnnotationAssert(final KtAnnotationEntry annotation) {
        return new MethodParameterAnnotationAssert(this, annotation);
    }

    public MethodAssert toMethod() {
        return methodAssert;
    }
}
