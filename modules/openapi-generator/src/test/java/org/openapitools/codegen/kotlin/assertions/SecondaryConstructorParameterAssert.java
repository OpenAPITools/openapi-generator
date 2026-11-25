package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtAnnotationEntry;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class SecondaryConstructorParameterAssert extends AbstractParameterAssert<SecondaryConstructorParameterAssert, SecondaryConstructorParameterAnnotationAssert> {
    private final SecondaryConstructorAssert secondaryConstructorAssert;

    SecondaryConstructorParameterAssert(final SecondaryConstructorAssert secondaryConstructorAssert, final KtParameter parameter) {
        super(parameter, SecondaryConstructorParameterAssert.class);
        this.secondaryConstructorAssert = secondaryConstructorAssert;
    }

    SecondaryConstructorParameterAnnotationAssert createAnnotationAssert(final KtAnnotationEntry annotation) {
        return new SecondaryConstructorParameterAnnotationAssert(this, annotation);
    }

    public SecondaryConstructorAssert toSecondaryConstructor() {
        return secondaryConstructorAssert;
    }
}
