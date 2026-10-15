package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;

@CanIgnoreReturnValue
public class SecondaryConstructorParameterAnnotationAssert extends AbstractAnnotationAssert<SecondaryConstructorParameterAnnotationAssert> {
    private final SecondaryConstructorParameterAssert parameterAssert;

    SecondaryConstructorParameterAnnotationAssert(final SecondaryConstructorParameterAssert parameterAssert, final KtAnnotationEntry annotationEntry) {
        super(annotationEntry, SecondaryConstructorParameterAnnotationAssert.class);
        this.parameterAssert = parameterAssert;
    }

    public SecondaryConstructorParameterAssert toSecondaryConstructorParameter() {
        return parameterAssert;
    }
}
