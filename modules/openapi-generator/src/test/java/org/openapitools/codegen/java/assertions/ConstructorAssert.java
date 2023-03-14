package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.nodeTypes.NodeWithName;
import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class ConstructorAssert extends AbstractAssert<ConstructorAssert, ConstructorDeclaration> {

    private final JavaFileAssert fileAssert;
    private final String signature;

    ConstructorAssert(final JavaFileAssert fileAssert, final ConstructorDeclaration constructorDeclaration) {
        super(constructorDeclaration, ConstructorAssert.class);
        this.fileAssert = fileAssert;
        this.signature = constructorDeclaration.getDeclarationAsString();
    }

    public JavaFileAssert toFileAssert() {
        return fileAssert;
    }

    public MethodAnnotationAssert assertConstructorAnnotations() {
        return new MethodAnnotationAssert(this, actual.getAnnotations());
    }

    public ParameterAssert hasParameter(final String paramName) {
        final Optional<Parameter> parameter = actual.getParameterByName(paramName);
        Assertions.assertThat(parameter)
            .withFailMessage("Constructor %s should have parameter %s, but it doesn't", signature, paramName)
            .isPresent();
        return new ParameterAssert(this, parameter.get());
    }

    public ConstructorAssert doesNotHaveParameter(final String paramName) {
        Assertions.assertThat(actual.getParameterByName(paramName))
            .withFailMessage("Constructor %s shouldn't have parameter %s, but it does", signature, paramName)
            .isEmpty();
        return this;
    }

    public ConstructorAssert bodyContainsLines(final String... lines) {
        final String actualBody = actual.getTokenRange()
            .orElseThrow(() -> new IllegalStateException("Can't get constructor body"))
            .toString();
        Assertions.assertThat(actualBody)
            .withFailMessage(
                "Constructor's %s body should contains lines\n====\n%s\n====\nbut actually was\n====\n%s\n====",
                signature, Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualBody
            )
            .contains(lines);

        return this;
    }

    public ConstructorAssert doesNotHaveComment() {
        Assertions.assertThat(actual.getJavadocComment())
            .withFailMessage("Constructor %s shouldn't contains comment, but it does", signature)
            .isEmpty();
        return this;
    }

    public ConstructorAssert commentContainsLines(final String... lines) {
        Assertions.assertThat(actual.getJavadocComment())
            .withFailMessage("Constructor %s should contains comment, but it doesn't", signature)
            .isPresent();
        final String actualComment = actual.getJavadocComment().get().getContent();
        Assertions.assertThat(actualComment)
            .withFailMessage(
                "Constructor's %s comment should contains lines\n====\n%s\n====\nbut actually was\n====%s\n====",
                signature, Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualComment
            )
            .contains(lines);

        return this;
    }

    public ConstructorAssert noneOfParameterHasAnnotation(final String annotationName) {
        actual.getParameters()
            .forEach(
                param -> Assertions.assertThat(param.getAnnotations())
                    .withFailMessage("Parameter %s contains annotation %s while it shouldn't", param.getNameAsString(), annotationName)
                    .extracting(NodeWithName::getNameAsString)
                    .doesNotContain(annotationName)
            );

        return this;
    }

}
