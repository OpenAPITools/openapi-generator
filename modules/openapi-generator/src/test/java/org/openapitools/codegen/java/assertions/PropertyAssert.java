package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.body.FieldDeclaration;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.ObjectAssert;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.regex.Pattern;

@CanIgnoreReturnValue
public class PropertyAssert extends ObjectAssert<FieldDeclaration> {

    private final JavaFileAssert javaFileAssert;

    protected PropertyAssert(final JavaFileAssert javaFileAssert, final FieldDeclaration fieldDeclaration) {
        super(fieldDeclaration);
        this.javaFileAssert = javaFileAssert;
    }

    public JavaFileAssert toType() {
        return javaFileAssert;
    }

    public PropertyAssert withType(final String expectedType) {
        Assertions.assertThat(actual.getElementType().toString())
                .withFailMessage("Expected property %s to have type %s, but was %s", actual.getVariable(0).getNameAsString(), expectedType, actual.getElementType().toString())
                .isEqualTo(expectedType);
        return this;
    }

    public PropertyAnnotationsAssert assertPropertyAnnotations() {
        return new PropertyAnnotationsAssert(this, actual.getAnnotations());
    }

    public PropertyAnnotationsAssert doesNotHaveAnnotation(String annotationName) {
        return new PropertyAnnotationsAssert(
                this,
                actual.getAnnotations()
        ).doesNotContainWithName(annotationName);
    }

    public PropertyAnnotationsAssert hasAnnotation(String annotationName) {
        return new PropertyAnnotationsAssert(
                this,
                actual.getAnnotations()
        ).containsWithName(annotationName);
    }

    /**
     * assert that the annotation is not specifed in an import.
     *
     * @param name classname of the annotation. For example "Nullable" or a full qualified class name like "java.util.List"
     */
    public PropertyAssert doesNotImportAnnotation(final String name) {
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
    public PropertyAssert doesImportAnnotation(final String name) {
        String pattern = "import\\s+" +
                (name.contains(".")?"" : "[\\w.]+\\.") +
                Pattern.quote(name) + ";";
        this.toType().fileContainsPattern(pattern);
        return this;
    }


}
