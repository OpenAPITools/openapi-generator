package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class FunctionParameterAnnotationAssert extends AbstractAnnotationAssert<FunctionParameterAnnotationAssert> {
    private final FunctionParameterAssert parameterAssert;

    FunctionParameterAnnotationAssert(final FunctionParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, FunctionParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public FunctionParameterAssert toParameter() {
        return parameterAssert;
    }
}
