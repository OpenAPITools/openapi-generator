package org.openapitools.codegen.java.assertions;

import java.util.List;

import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.ast.expr.AnnotationExpr;

@CanIgnoreReturnValue
public class ParameterAnnotationsAssert extends AbstractAnnotationsAssert<ParameterAnnotationsAssert> {

    private final ParameterAssert parameterAssert;

    protected ParameterAnnotationsAssert(final ParameterAssert parameterAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.parameterAssert = parameterAssert;
    }

    public ParameterAssert toParameter() {
        return parameterAssert;
    }
}
