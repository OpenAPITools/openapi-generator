package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.expr.AnnotationExpr;
import java.util.List;
import org.assertj.core.util.CanIgnoreReturnValue;

@CanIgnoreReturnValue
public class TypeAnnotationAssert extends AbstractAnnotationAssert<TypeAnnotationAssert> {

    private final JavaFileAssert fileAssert;

    protected TypeAnnotationAssert(
            final JavaFileAssert fileAssert, final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
        this.fileAssert = fileAssert;
    }

    public JavaFileAssert toType() {
        return fileAssert;
    }
}
