package org.openapitools.codegen.java.assertions;

import java.util.List;

import com.github.javaparser.ast.expr.AnnotationExpr;

public class PropertyAnnotationAssert extends AbstractAnnotationAssert<PropertyAnnotationAssert> {

    private final PropertyAssert propertyAssert;

    protected PropertyAnnotationAssert(final PropertyAssert propertyAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.propertyAssert = propertyAssert;
    }

    public PropertyAssert toProperty() {
        return propertyAssert;
    }
}
