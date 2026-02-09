package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.body.FieldDeclaration;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.ObjectAssert;
import org.assertj.core.util.CanIgnoreReturnValue;

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

    public PropertyAssert isArray() {
        Assertions.assertThat(actual.getCommonType().isArrayType())
                .withFailMessage("Expected property %s to be array, but it was NOT", actual.getVariable(0).getNameAsString())
                .isEqualTo(true);
        return this;
    }

    public PropertyAssert isNotArray() {
        Assertions.assertThat(actual.getCommonType().isArrayType())
                .withFailMessage("Expected property %s NOT to be array, but it was", actual.getVariable(0).getNameAsString())
                .isEqualTo(false);
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
}
