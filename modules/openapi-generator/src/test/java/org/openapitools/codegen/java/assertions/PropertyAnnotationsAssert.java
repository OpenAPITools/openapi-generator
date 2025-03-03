package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.expr.AnnotationExpr;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.List;

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
