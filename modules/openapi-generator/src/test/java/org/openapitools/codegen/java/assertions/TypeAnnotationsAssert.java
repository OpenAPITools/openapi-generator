package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.expr.AnnotationExpr;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.List;

@CanIgnoreReturnValue
public class TypeAnnotationsAssert extends AbstractAnnotationsAssert<TypeAnnotationsAssert> {

    private final JavaFileAssert fileAssert;

    protected TypeAnnotationsAssert(final JavaFileAssert fileAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.fileAssert = fileAssert;
    }

    public JavaFileAssert toType() {
        return fileAssert;
    }
}
