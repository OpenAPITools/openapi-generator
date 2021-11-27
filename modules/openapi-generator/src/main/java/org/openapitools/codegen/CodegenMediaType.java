package org.openapitools.codegen;

import java.util.Map;
import java.util.Objects;

public class CodegenMediaType {
    private CodegenProperty schema;
    private Map<String, CodegenEncoding> content;

    public CodegenMediaType(CodegenProperty schema, Map<String, CodegenEncoding> content) {
        this.schema = schema;
        this.content = content;
    }

    public CodegenProperty getSchema() {
        return schema;
    }

    public Map<String, CodegenEncoding> getContent() {
        return content;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenMediaType{");
        sb.append("schema=").append(schema);
        sb.append(", content=").append(content);
        sb.append('}');
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenMediaType that = (CodegenMediaType) o;
        return Objects.equals(schema,that.getSchema()) &&
                Objects.equals(content, that.getContent());
    }

    @Override
    public int hashCode() {
        return Objects.hash(schema, content);
    }
}


