package org.openapitools.codegen.java.assertions;

import java.util.List;

import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.ast.expr.AnnotationExpr;

@CanIgnoreReturnValue
public class TypeAnnotationAssert extends AbstractAnnotationAssert<TypeAnnotationAssert> {

    private final JavaFileAssert fileAssert;

    protected TypeAnnotationAssert(final JavaFileAssert fileAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.fileAssert = fileAssert;
    }

    public JavaFileAssert toType() {
        return fileAssert;
    }
}
