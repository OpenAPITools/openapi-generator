package org.openapitools.codegen.v2.outputs;

import java.util.Objects;

public final class CodegenOutput {
    private final Object location;
    private final String content;

    public CodegenOutput(Object location, String content) {
        this.location = Objects.requireNonNull(location);
        this.content = Objects.requireNonNull(content);
    }

    public Object getLocation() {
        return location;
    }

    public String getContent() {
        return content;
    }
}
