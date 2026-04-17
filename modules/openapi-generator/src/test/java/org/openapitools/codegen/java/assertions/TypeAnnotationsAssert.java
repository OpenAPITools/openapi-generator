package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.expr.AnnotationExpr;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.List;
import java.util.regex.Pattern;

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

    /**
     * assert that the annotation is not specifed in an import.
     *
     * @param name name of the annotation. For example "Nullable";
     */
    public TypeAnnotationsAssert doesNotImportAnnotation(final String name) {
        String pattern = "import\\s+[\\w.]+\\." + Pattern.quote(name) + ";";
        this.toType().fileDoesNotContainPattern(pattern);
        return this;
    }

    /**
     * assert that the annotation is imported.
     *
     * @param name name of the annotation. For example "Nullable";
     */
    public TypeAnnotationsAssert doesImportAnnotation(final String name) {
        String pattern = "import\\s+[\\w.]+\\." + Pattern.quote(name) + ";";
        this.toType().fileContainsPattern(pattern);
        return this;
    }
}
