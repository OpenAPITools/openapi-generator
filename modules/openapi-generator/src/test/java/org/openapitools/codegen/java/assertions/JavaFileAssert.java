package org.openapitools.codegen.java.assertions;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.nodeTypes.NodeWithName;
import com.github.javaparser.ast.nodeTypes.modifiers.NodeWithAbstractModifier;

@CanIgnoreReturnValue
public class JavaFileAssert extends AbstractAssert<JavaFileAssert, CompilationUnit> {

    private JavaFileAssert(final CompilationUnit actual) {
        super(actual, JavaFileAssert.class);
    }

    public static JavaFileAssert assertThat(final String source) {
        return new JavaFileAssert(StaticJavaParser.parse(source));
    }

    public static JavaFileAssert assertThat(final Path path) {
        try {
            return new JavaFileAssert(StaticJavaParser.parse(path));
        } catch (IOException e) {
            throw new RuntimeException("Exception while reading file: " + path, e);
        }
    }

    public static JavaFileAssert assertThat(final File file) {
        try {
            return new JavaFileAssert(StaticJavaParser.parse(file));
        } catch (IOException e) {
            throw new RuntimeException("Exception while reading file: " + file, e);
        }
    }

    public JavaFileAssert isInterface() {
        Assertions.assertThat(actual.getType(0).asClassOrInterfaceDeclaration())
            .withFailMessage("Expected type %s to be an interface", actual.getType(0).getName().asString())
            .extracting(ClassOrInterfaceDeclaration::isInterface)
            .isEqualTo(true);
        return this;
    }

    public JavaFileAssert isNormalClass() {
        Assertions.assertThat(actual.getType(0).asClassOrInterfaceDeclaration())
            .withFailMessage("Expected type %s to be a normal class(non-abstract)", actual.getType(0).getName().asString())
            .extracting(ClassOrInterfaceDeclaration::isInterface, NodeWithAbstractModifier::isAbstract)
            .containsExactly(false, false);
        return this;
    }

    public JavaFileAssert isAbstractClass() {
        Assertions.assertThat(actual.getType(0).asClassOrInterfaceDeclaration())
            .withFailMessage("Expected type %s to be an abstract class", actual.getType(0).getName().asString())
            .extracting(ClassOrInterfaceDeclaration::isInterface, NodeWithAbstractModifier::isAbstract)
            .containsExactly(false, true);
        return this;
    }

    public MethodAssert assertMethod(final String methodName, final String... paramTypes) {
        List<MethodDeclaration> methods = paramTypes.length == 0
            ? actual.getType(0).getMethodsByName(methodName)
            : actual.getType(0).getMethodsBySignature(methodName, paramTypes);
        String message = paramTypes.length == 0
            ? "Expected to be a single method %s, but found " + methods.size()
            : "Expected to be a single method %s with parameter(s) %s, but found " + methods.size();
        Assertions.assertThat(methods)
            .withFailMessage(message, methodName, Arrays.toString(paramTypes))
            .hasSize(1);

        return new MethodAssert(this, methods.get(0));
    }

    public ConstructorAssert assertConstructor(final String... paramTypes) {
        Optional<ConstructorDeclaration> constructorDeclaration = actual.getType(0).getConstructorByParameterTypes(paramTypes);
        Assertions.assertThat(constructorDeclaration)
            .withFailMessage("No constructor with parameter(s) %s", Arrays.toString(paramTypes))
            .isPresent();

        return new ConstructorAssert(this, constructorDeclaration.get());
    }

    public JavaFileAssert assertNoConstructor(final String... paramTypes) {
        Optional<ConstructorDeclaration> constructorDeclaration = actual.getType(0).getConstructorByParameterTypes(paramTypes);
        Assertions.assertThat(constructorDeclaration)
                .withFailMessage("Found constructor with parameter(s) %s", Arrays.toString(paramTypes))
                .isEmpty();

        return this;
    }

    public PropertyAssert hasProperty(final String propertyName) {
        Optional<FieldDeclaration> fieldOptional = actual.getType(0).getMembers().stream()
            .filter(FieldDeclaration.class::isInstance)
            .map(FieldDeclaration.class::cast)
            .filter(field -> field.getVariables().getFirst().map(var -> var.getNameAsString().equals(propertyName)).orElse(Boolean.FALSE))
            .findFirst();
        Assertions.assertThat(fieldOptional)
            .withFailMessage("Should have field with name %s", propertyName)
            .isPresent();

        return new PropertyAssert(this, fieldOptional.get());
    }

    public JavaFileAssert hasImports(final String... imports) {
        Assertions.assertThat(actual.getImports().stream().map(NodeWithName::getNameAsString))
            .containsAll(Arrays.asList(imports));
        return this;
    }

    public JavaFileAssert hasNoImports(final String... imports) {
        Assertions.assertThat(actual.getImports().stream().map(NodeWithName::getNameAsString))
            .doesNotContainAnyElementsOf(Arrays.asList(imports));
        return this;
    }

    public JavaFileAssert printFileContent() {
        System.out.println(actual);
        return this;
    }

    public JavaFileAssert fileContains(final String... lines) {
        final String actualBody = actual.getTokenRange()
            .orElseThrow(() -> new IllegalStateException("Empty file"))
            .toString();
        Assertions.assertThat(actualBody)
            .withFailMessage(
                "File should contains lines\n====\n%s\n====\nbut actually was\n====\n%s\n====",
                Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator())), actualBody
            )
            .contains(lines);

        return this;
    }

    public TypeAnnotationAssert assertTypeAnnotations() {
        return new TypeAnnotationAssert(this, actual.getType(0).getAnnotations());
    }

}
