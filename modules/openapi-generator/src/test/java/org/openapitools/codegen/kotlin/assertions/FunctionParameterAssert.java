package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class FunctionParameterAssert extends AbstractParameterAssert<FunctionParameterAssert, FunctionParameterAnnotationAssert> {
    private final FunctionAssert functionAssert;

    FunctionParameterAssert(final FunctionAssert functionAssert, final KtParameter parameter) {
        super(parameter, FunctionParameterAssert.class);
        this.functionAssert = functionAssert;
    }

    FunctionParameterAnnotationAssert createAnnotationAssert(final KtAnnotationEntry annotation) {
        return new FunctionParameterAnnotationAssert(this, annotation);
    }

    public FunctionAssert toFunction() {
        return functionAssert;
    }
}
