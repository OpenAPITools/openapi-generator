package org.openapitools.codegen.java.assertions;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.ObjectAssert;

import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.Parameter;

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

    public PropertyAnnotationAssert assertPropertyAnnotations() {
        return new PropertyAnnotationAssert(this, actual.getAnnotations());
    }
}
