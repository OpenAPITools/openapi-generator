package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class MethodAssert extends AbstractFunctionAssert<MethodAssert, MethodParameterAssert> {
    private final ClassAssert classAssert;

    MethodAssert(final ClassAssert classAssert, final KtNamedFunction method) {
        super(method, MethodAssert.class);
        this.classAssert = classAssert;
    }

    MethodParameterAssert createParameterAssert(final KtParameter parameter) {
        return new MethodParameterAssert(this, parameter);
    }

    public ClassAssert toClass() {
        return classAssert;
    }
}
