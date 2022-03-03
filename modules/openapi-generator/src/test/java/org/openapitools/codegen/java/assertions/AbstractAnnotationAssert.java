package org.openapitools.codegen.java.assertions;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.assertj.core.api.ListAssert;

import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.MemberValuePair;
import com.github.javaparser.ast.nodeTypes.NodeWithSimpleName;

public abstract class AbstractAnnotationAssert<ACTUAL extends AbstractAnnotationAssert<ACTUAL>> extends ListAssert<AnnotationExpr> {

    protected AbstractAnnotationAssert(final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
    }

    public ACTUAL hasSize(final int size) {
        super.hasSize(size);
        return myself();
    }

    public ACTUAL containsWithName(final String name) {
        super
            .withFailMessage("Should have annotation with name: " + name)
            .anyMatch(annotation -> annotation.getNameAsString().equals(name));
        return myself();
    }

    public ACTUAL containsWithNameAndAttributes(final String name, final Map<String, String> attributes) {
        super
            .withFailMessage("Should have annotation with name: " + name + " and attributes: " + attributes + ", but was: " + actual)
            .anyMatch(annotation -> annotation.getNameAsString().equals(name) && hasAttributes(annotation, attributes));
        return myself();
    }

    private static boolean hasAttributes(final AnnotationExpr annotation, final Map<String, String> expectedAttributesToContains) {
        final Map<String, String> actualAttributes = annotation.getChildNodes().stream()
            .filter(MemberValuePair.class::isInstance)
            .map(MemberValuePair.class::cast)
            .collect(Collectors.toMap(NodeWithSimpleName::getNameAsString, pair -> pair.getValue().toString()));

        return expectedAttributesToContains.entrySet().stream()
            .allMatch(expected -> Objects.equals(actualAttributes.get(expected.getKey()), expected.getValue()));
    }

    @SuppressWarnings("unchecked")
    private ACTUAL myself() {
        return (ACTUAL) this;
    }
}
