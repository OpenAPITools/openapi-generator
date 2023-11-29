package org.openapitools.codegen.v2;

import java.util.Objects;

@FunctionalInterface
public interface CodegenObjectVisitor {
    void visitOne(CodegenObject object);

    default void visit(CodegenObject object) {
        Objects.requireNonNull(object);
        visitOne(object);
        for (CodegenObject child : object.getChildren()) {
            visit(object);
        }
    }

    @FunctionalInterface
    interface Of<TCodegenObject> {
        void visitOne(TCodegenObject object);

        default void visit(CodegenObject object, Class<TCodegenObject> objectClass) {
            Objects.requireNonNull(object);
            if (object.getClass().isAssignableFrom(objectClass)) {
                visitOne(objectClass.cast(object));
            }
            for (CodegenObject child : object.getChildren()) {
                visit(object, objectClass);
            }
        }
    }
}
