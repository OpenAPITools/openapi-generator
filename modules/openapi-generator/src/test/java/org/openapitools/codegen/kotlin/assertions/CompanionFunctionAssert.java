package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class CompanionFunctionAssert extends AbstractFunctionAssert<CompanionFunctionAssert, CompanionFunctionParameterAssert> {
    private final ClassAssert classAssert;

    CompanionFunctionAssert(final ClassAssert classAssert, final KtNamedFunction function) {
        super(function, CompanionFunctionAssert.class);
        this.classAssert = classAssert;
    }

    CompanionFunctionParameterAssert createParameterAssert(final KtParameter parameter) {
        return new CompanionFunctionParameterAssert(this, parameter);
    }

    public ClassAssert toClass() {
        return classAssert;
    }
}
