package org.openapitools.codegen.java.assertions;

import java.util.List;

import com.github.javaparser.ast.expr.AnnotationExpr;

public class ParameterAnnotationAssert extends AbstractAnnotationAssert<ParameterAnnotationAssert> {

    private final ParameterAssert parameterAssert;

    protected ParameterAnnotationAssert(final ParameterAssert parameterAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.parameterAssert = parameterAssert;
    }

    public ParameterAssert toParameter() {
        return parameterAssert;
    }
}
