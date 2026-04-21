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
     * @param name classname of the annotation. For example "Nullable" or a full qualified class name like "java.util.List"
     */
    public TypeAnnotationsAssert doesNotImportAnnotation(final String name) {
        String pattern = "import\\s+" +
                (name.contains(".")?"" : "[\\w.]+\\.") +
                Pattern.quote(name) + ";";
        this.toType().fileDoesNotContainPattern(pattern);
        return this;
    }

    /**
     * assert that the annotation is imported.
     *
     * @param name clasname of the annotation.  For example "Nullable" or a full qualified class name like "java.util.List"
     */
    public TypeAnnotationsAssert doesImportAnnotation(final String name) {
        String pattern = "import\\s+" +
                (name.contains(".")?"" : "[\\w.]+\\.") +
                Pattern.quote(name) + ";";
        this.toType().fileContainsPattern(pattern);
        return this;
    }
}
