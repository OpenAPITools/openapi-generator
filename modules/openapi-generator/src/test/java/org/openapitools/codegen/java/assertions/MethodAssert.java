package org.openapitools.codegen.java.assertions;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.nodeTypes.NodeWithName;

public class MethodAssert extends AbstractAssert<MethodAssert, MethodDeclaration> {

    private final JavaFileAssert fileAssert;
    private final String methodSignature;

    MethodAssert(final JavaFileAssert fileAssert, final MethodDeclaration methodDeclaration) {
        super(methodDeclaration, MethodAssert.class);
        this.fileAssert = fileAssert;
        this.methodSignature = methodDeclaration.getDeclarationAsString();
    }

    public JavaFileAssert toFileAssert() {
        return fileAssert;
    }

    public MethodAnnotationAssert assertMethodAnnotations() {
        return new MethodAnnotationAssert(this, actual.getAnnotations());
    }

    public MethodAssert hasReturnType(final String returnType) {
        Assertions.assertThat(actual.getType().toString())
            .isEqualTo(returnType);
        return this;
    }

    public ParameterAssert hasParameter(final String paramName) {
        final Optional<Parameter> parameter = actual.getParameterByName(paramName);
        Assertions.assertThat(parameter)
            .withFailMessage("Method %s should have parameter %s, but it doesn't", methodSignature, paramName)
            .isPresent();
        return new ParameterAssert(this, parameter.get());
    }

    public MethodAssert doesNotHaveParameter(final String paramName) {
        Assertions.assertThat(actual.getParameterByName(paramName))
            .withFailMessage("Method %s shouldn't have parameter %s, but it does", methodSignature, paramName)
            .isEmpty();
        return this;
    }

    public MethodAssert bodyContainsLines(final String... lines) {
        Assertions.assertThat(isWithImplementation())
            .withFailMessage("Method %s is abstract", methodSignature)
            .isTrue();
        final String actualBody = actual.getTokenRange()
            .orElseThrow(() -> new IllegalStateException("Not-abstract method doesn't have body"))
            .toString();
        Assertions.assertThat(actualBody)
            .withFailMessage(
                "Method's %s body should contains lines\n====\n%s\n====\nbut actually was\n====\n%s\n====",
                methodSignature, Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualBody
            )
            .contains(lines);

        return this;
    }

    public MethodAssert doesNotHaveImplementation() {
        Assertions.assertThat(isWithImplementation())
            .withFailMessage("Method %s should be abstract", methodSignature)
            .isFalse();
        return this;
    }

    public MethodAssert doesNotHaveComment() {
        Assertions.assertThat(actual.getJavadocComment())
            .withFailMessage("Method %s shouldn't contains comment, but it does", methodSignature)
            .isEmpty();
        return this;
    }

    public MethodAssert commentContainsLines(final String... lines) {
        Assertions.assertThat(actual.getJavadocComment())
            .withFailMessage("Method %s should contains comment, but it doesn't", methodSignature)
            .isPresent();
        final String actualComment = actual.getJavadocComment().get().getContent();
        Assertions.assertThat(actualComment)
            .withFailMessage(
                "Method's %s comment should contains lines\n====\n%s\n====\nbut actually was\n====%s\n====",
                methodSignature, Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualComment
            )
            .contains(lines);

        return this;
    }

    public MethodAssert noneOfParameterHasAnnotation(final String annotationName) {
        actual.getParameters()
            .forEach(
                param -> Assertions.assertThat(param.getAnnotations())
                    .withFailMessage("Parameter %s contains annotation %s while it shouldn't", param.getNameAsString(), annotationName)
                    .extracting(NodeWithName::getNameAsString)
                    .doesNotContain(annotationName)
            );

        return this;
    }

    private boolean isWithImplementation() {
        final boolean isInterface = actual.getParentNode()
            .filter(ClassOrInterfaceDeclaration.class::isInstance)
            .map(ClassOrInterfaceDeclaration.class::cast)
            .map(ClassOrInterfaceDeclaration::isInterface)
            .orElse(false);
        return !(actual.isAbstract() || (isInterface && !actual.isDefault()));
    }

}
