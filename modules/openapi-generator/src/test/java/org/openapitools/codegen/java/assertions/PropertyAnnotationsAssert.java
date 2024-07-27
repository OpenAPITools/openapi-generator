package org.openapitools.codegen.java.assertions;

import java.util.List;

import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.ast.expr.AnnotationExpr;

@CanIgnoreReturnValue
public class PropertyAnnotationsAssert extends AbstractAnnotationsAssert<PropertyAnnotationsAssert> {

    private final PropertyAssert propertyAssert;

    protected PropertyAnnotationsAssert(final PropertyAssert propertyAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.propertyAssert = propertyAssert;
    }

    public PropertyAssert toProperty() {
        return propertyAssert;
    }
}
