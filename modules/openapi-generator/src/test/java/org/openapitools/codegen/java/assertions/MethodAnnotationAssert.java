package org.openapitools.codegen.java.assertions;

import java.util.List;

import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.ast.expr.AnnotationExpr;

@CanIgnoreReturnValue
public class MethodAnnotationAssert extends AbstractAnnotationAssert<MethodAnnotationAssert> {

    private final MethodAssert methodAssert;

    protected MethodAnnotationAssert(final MethodAssert methodAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.methodAssert = methodAssert;
    }

    public MethodAssert toMethod() {
        return methodAssert;
    }
}
