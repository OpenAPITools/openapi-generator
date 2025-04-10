package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.expr.AnnotationExpr;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.List;

@CanIgnoreReturnValue
public class MethodAnnotationsAssert extends AbstractAnnotationsAssert<MethodAnnotationsAssert> {

    private final MethodAssert methodAssert;
    private final ConstructorAssert constructorAssert;

    protected MethodAnnotationsAssert(final MethodAssert methodAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.methodAssert = methodAssert;
        this.constructorAssert = null;
    }

    protected MethodAnnotationsAssert(final ConstructorAssert constructorAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.constructorAssert = constructorAssert;
        this.methodAssert = null;
    }

    public MethodAssert toMethod() {
        if (methodAssert == null) {
            throw new IllegalArgumentException("No method assert for constructor's annotations");
        }
        return methodAssert;
    }

    public ConstructorAssert toConstructor() {
        if (constructorAssert == null) {
            throw new IllegalArgumentException("No constructor assert for method's annotations");
        }
        return constructorAssert;
    }

}
