package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.psi.KtNamedFunction;
import org.jetbrains.kotlin.psi.KtParameter;

@CanIgnoreReturnValue
public class FunctionAssert extends AbstractFunctionAssert<FunctionAssert, FunctionParameterAssert> {
    private final KotlinFileAssert fileAssert;

    FunctionAssert(final KotlinFileAssert fileAssert, final KtNamedFunction function) {
        super(function, FunctionAssert.class);
        this.fileAssert = fileAssert;
    }

    FunctionParameterAssert createParameterAssert(final KtParameter parameter) {
        return new FunctionParameterAssert(this, parameter);
    }

    public KotlinFileAssert toFile() {
        return fileAssert;
    }
}
