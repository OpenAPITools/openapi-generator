package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class CompanionFunctionParameterAssert extends AbstractParameterAssert<CompanionFunctionParameterAssert, CompanionFunctionParameterAnnotationAssert> {
    private final CompanionFunctionAssert functionAssert;

    CompanionFunctionParameterAssert(final CompanionFunctionAssert functionAssert, final KtParameter parameter) {
        super(parameter, CompanionFunctionParameterAssert.class);
        this.functionAssert = functionAssert;
    }

    CompanionFunctionParameterAnnotationAssert createAnnotationAssert(final KtAnnotationEntry annotation) {
        return new CompanionFunctionParameterAnnotationAssert(this, annotation);
    }

    public CompanionFunctionAssert toFunction() {
        return functionAssert;
    }
}
