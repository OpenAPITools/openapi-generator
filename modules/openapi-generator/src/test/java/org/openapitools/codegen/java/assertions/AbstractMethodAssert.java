package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.nodeTypes.NodeWithName;
import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public abstract class AbstractMethodAssert<ACTUAL extends AbstractMethodAssert<ACTUAL>> extends AbstractAssert<AbstractMethodAssert<ACTUAL>, MethodDeclaration> {

    protected final String methodSignature;

    protected AbstractMethodAssert(MethodDeclaration methodDeclaration, Class<?> selfType) {
        super(methodDeclaration, selfType);
        this.methodSignature = methodDeclaration.getDeclarationAsString();
    }

    public abstract JavaFileAssert toFileAssert();

    public abstract AbstractAnnotationsAssert<?> assertMethodAnnotations();

    public ACTUAL hasReturnType(final String returnType) {
        Assertions.assertThat(actual.getType().toString())
                .isEqualTo(returnType);
        return myself();
    }

    public ACTUAL hasAnnotation(String annotation) {
        assertMethodAnnotations().containsWithName(annotation);
        return myself();
    }

    public ACTUAL hasAnnotation(String annotation, final Map<String, String> attributes) {
        assertMethodAnnotations().containsWithNameAndAttributes(annotation, attributes);
        return myself();
    }

    public ACTUAL doesNotHaveAnnotation(String annotation) {
        assertMethodAnnotations().doesNotContainWithName(annotation);
        return myself();
    }

    public ACTUAL doesNotHaveParameter(final String paramName) {
        Assertions.assertThat(actual.getParameterByName(paramName))
                .withFailMessage("Method %s shouldn't have parameter %s, but it does", methodSignature, paramName)
                .isEmpty();
        return myself();
    }

    public ACTUAL doesNotHaveParameters() {
        Assertions.assertThat(actual.getParameters())
                .withFailMessage("Method %s shouldn't have parameter, but it does", methodSignature)
                .isEmpty();
        return myself();
    }

    public ACTUAL bodyContainsLines(final String... lines) {
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

        return myself();
    }

    public ACTUAL bodyNotContainsLines(final String... lines) {
        Assertions.assertThat(isWithImplementation())
                .withFailMessage("Method %s is abstract", methodSignature)
                .isTrue();
        final String actualBody = actual.getTokenRange()
                .orElseThrow(() -> new IllegalStateException("Not-abstract method doesn't have body"))
                .toString();
        Assertions.assertThat(actualBody)
                .withFailMessage(
                        "Method's %s body shouldn't contains lines\n====\n%s\n====\nbut actually was\n====\n%s\n====",
                        methodSignature, Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualBody
                )
                .doesNotContain(lines);

        return myself();
    }

    public ACTUAL doesNotHaveImplementation() {
        Assertions.assertThat(isWithImplementation())
                .withFailMessage("Method %s should be abstract", methodSignature)
                .isFalse();
        return myself();
    }

    public ACTUAL doesNotHaveComment() {
        Assertions.assertThat(actual.getJavadocComment())
                .withFailMessage("Method %s shouldn't contains comment, but it does", methodSignature)
                .isEmpty();
        return myself();
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    public ACTUAL commentContainsLines(final String... lines) {
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

        return myself();
    }

    public ACTUAL noneOfParameterHasAnnotation(final String annotationName) {
        actual.getParameters()
                .forEach(
                        param -> Assertions.assertThat(param.getAnnotations())
                                .withFailMessage("Parameter %s contains annotation %s while it shouldn't", param.getNameAsString(), annotationName)
                                .extracting(NodeWithName::getNameAsString)
                                .doesNotContain(annotationName)
                );

        return myself();
    }

    private boolean isWithImplementation() {
        final boolean isInterface = actual.getParentNode()
                .filter(ClassOrInterfaceDeclaration.class::isInstance)
                .map(ClassOrInterfaceDeclaration.class::cast)
                .map(ClassOrInterfaceDeclaration::isInterface)
                .orElse(false);
        return !(actual.isAbstract() || (isInterface && !actual.isDefault()));
    }

    @SuppressWarnings("unchecked")
    private ACTUAL myself() {
        return (ACTUAL) this;
    }
}
