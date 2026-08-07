package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class CompanionFunctionParameterAnnotationAssert extends AbstractAnnotationAssert<CompanionFunctionParameterAnnotationAssert> {
    private final CompanionFunctionParameterAssert parameterAssert;

    CompanionFunctionParameterAnnotationAssert(final CompanionFunctionParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, CompanionFunctionParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public CompanionFunctionParameterAssert toParameter() {
        return parameterAssert;
    }
}
