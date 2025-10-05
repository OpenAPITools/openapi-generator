package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.MarkerAnnotationExpr;
import com.github.javaparser.ast.expr.NormalAnnotationExpr;
import com.github.javaparser.ast.expr.SingleMemberAnnotationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithSimpleName;
import com.google.common.collect.ImmutableMap;
import org.assertj.core.api.ListAssert;
import org.assertj.core.util.CanIgnoreReturnValue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public abstract class AbstractAnnotationsAssert<ACTUAL extends AbstractAnnotationsAssert<ACTUAL>> extends ListAssert<AnnotationExpr> {

    protected AbstractAnnotationsAssert(final List<AnnotationExpr> annotationExpr) {
        super(annotationExpr);
    }

    @Override
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

    public ACTUAL doesNotContainWithName(final String name) {
        super
                .withFailMessage("Shouldn't have annotation with name: " + name)
                .noneMatch(annotation -> annotation.getNameAsString().equals(name));
        return myself();
    }

    public ACTUAL containsWithNameAndAttributes(final String name, final Map<String, String> attributes) {
        super
                .withFailMessage("Should have annotation with name: " + name + " and attributes: " + attributes + ", but was: " + actual)
                .anyMatch(annotation -> annotation.getNameAsString().equals(name) && hasAttributes(annotation, attributes));
        return myself();
    }

    public ACTUAL containsWithNameAndDoesContainAttributes(final String name, final List<String> attributes) {
        super
                .withFailMessage("Should have annotation with name: " + name + " and no attributes: " + attributes + ", but was: " + actual)
                .anyMatch(annotation -> annotation.getNameAsString().equals(name) && hasNotAttributes(annotation, attributes));
        return myself();
    }

    private static boolean hasNotAttributes(final AnnotationExpr annotation, final List<String> attributes) {
        final Map<String, String> actualAttributes = getAttributes(annotation);

        return actualAttributes.keySet().stream()
                .noneMatch(attribute -> attributes.contains(attribute));
    }

    private static boolean hasAttributes(final AnnotationExpr annotation, final Map<String, String> expectedAttributesToContains) {
        final Map<String, String> actualAttributes = getAttributes(annotation);

        return expectedAttributesToContains.entrySet().stream()
                .allMatch(expected -> Objects.equals(actualAttributes.get(expected.getKey()), expected.getValue()));
    }

    private static Map<String, String> getAttributes(final AnnotationExpr annotation) {
        if (annotation instanceof SingleMemberAnnotationExpr) {
            return ImmutableMap.of(
                    "value", ((SingleMemberAnnotationExpr) annotation).getMemberValue().toString()
            );
        } else if (annotation instanceof NormalAnnotationExpr) {
            return ((NormalAnnotationExpr) annotation).getPairs().stream()
                    .collect(Collectors.toMap(NodeWithSimpleName::getNameAsString, pair -> pair.getValue().toString()));
        } else if (annotation instanceof MarkerAnnotationExpr) {
            return new HashMap<>();
        } else {
            throw new IllegalArgumentException("Unexpected annotation expression type for: " + annotation);
        }
    }

    @SuppressWarnings("unchecked")
    private ACTUAL myself() {
        return (ACTUAL) this;
    }

    public ACTUAL recursivelyContainsWithName(String name) {
        super
            .withFailMessage("Should have annotation with name: " + name)
            .anyMatch(annotation -> containsSpecificAnnotationName(annotation, name));

        return myself();
    }

    private boolean containsSpecificAnnotationName(Node node, String name) {
        if (node == null || name == null)
            return false;

        if (node instanceof AnnotationExpr) {
            AnnotationExpr annotation = (AnnotationExpr) node;

            if(annotation.getNameAsString().equals(name))
                return true;

        }

        for(Node child: node.getChildNodes()){
            if(containsSpecificAnnotationName(child, name))
                return true;
        }

        return false;
    }
}
