package org.openapitools.codegen.java.assertions;

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.AnnotationExpr;
import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;

import java.util.Arrays;
import java.util.List;

public class InnerClassAssert extends AbstractAssert<InnerClassAssert, ClassOrInterfaceDeclaration> {

    private final JavaFileAssert parent;
    private final ClassOrInterfaceDeclaration innerClass;

    protected InnerClassAssert(final JavaFileAssert fileAssert, final ClassOrInterfaceDeclaration bodyDeclaration) {
        super(bodyDeclaration, InnerClassAssert.class);
        parent = fileAssert;
        innerClass = bodyDeclaration;
    }

    public JavaFileAssert toFileAssert() {
        return parent;
    }

    // TODO this effectively duplicates JavaFileAssert.assertMethod, could be moved into a common
    //  intermediate base class (which would then easily implement property, type & constructor 
    //  assertions for inner classes
    public MethodAssert assertMethod(final String methodName, final String... paramTypes) {

        final List<MethodDeclaration> methods = paramTypes.length == 0
                ? innerClass.getMethodsByName(methodName)
                : innerClass.getMethodsBySignature(methodName, paramTypes);

        final String failMessage = (methods.isEmpty()
                ? "No methods matching `%s(%s)` exist"
                : "There are " + methods.size() + " methods matching `%s(%s)`")
                + " in inner class `" + innerClass.getName() + "`";

        Assertions.assertThat(methods)
                .withFailMessage(failMessage, methodName, Arrays.toString(paramTypes).replaceAll("\\[]", ""))
                .hasSize(1);

        return new MethodAssert(this, methods.get(0));
    }

    public static class MethodAssert extends AbstractMethodAssert<MethodAssert> {

        private final InnerClassAssert innerClassAssert;

        MethodAssert(InnerClassAssert innerClassAssert, MethodDeclaration methodDeclaration) {
            super(methodDeclaration, MethodAssert.class);
            this.innerClassAssert = innerClassAssert;
        }

        public MethodAnnotationsAssert assertMethodAnnotations() {
            return new MethodAnnotationsAssert(this, actual.getAnnotations());
        }

        public InnerClassAssert toInnerClassAssert() {
            return innerClassAssert;
        }

        public JavaFileAssert toFileAssert() {
            return innerClassAssert.toFileAssert();
        }
    }

    public static class MethodAnnotationsAssert extends AbstractAnnotationsAssert<MethodAnnotationsAssert> {

        private final MethodAssert parent;

        protected MethodAnnotationsAssert(MethodAssert methodAssert, List<AnnotationExpr> annotationExpr) {
            super(annotationExpr);
            parent = methodAssert;
        }

        public InnerClassAssert toInnerClassAssert() {
            return parent.innerClassAssert;
        }

        public MethodAssert toMethod() {
            return parent;
        }
    }
}
