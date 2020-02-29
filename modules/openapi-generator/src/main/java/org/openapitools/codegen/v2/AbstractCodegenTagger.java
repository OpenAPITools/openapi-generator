package org.openapitools.codegen.v2;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.Objects;

public abstract class AbstractCodegenTagger implements CodegenTagger {
    @Override
    public final void tag(CodegenObject object) {
        Objects.requireNonNull(object);
        try {
            Class<? extends CodegenObject> objectClass = object.getClass();
            Method tagMethod = getClass().getDeclaredMethod("tag", objectClass);
            tagMethod.invoke(this, objectClass.cast(object));
        } catch (ReflectiveOperationException e) {
            // no-op: no tag method for object
        }
    }

    protected void tag(CodegenSdk sdk) {
        // virtual no-op
    }

    protected void tag(CodegenApi api) {
        // virtual no-op
    }

    protected void tag(CodegenModel model) {
        // virtual no-op
    }

    protected void tag(CodegenProperty property) {
        // virtual no-op
    }

    protected void tag(CodegenOperation operation) {
        // virtual no-op
    }

    protected void tag(CodegenParameter parameter) {
        // virtual no-op
    }
}
