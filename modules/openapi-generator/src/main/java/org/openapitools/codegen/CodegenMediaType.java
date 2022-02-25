package org.openapitools.codegen;

import java.util.LinkedHashMap;
import java.util.Objects;

public class CodegenMediaType {
    private CodegenProperty schema;
    private LinkedHashMap<String, CodegenEncoding> encoding;

    public CodegenMediaType(CodegenProperty schema, LinkedHashMap<String, CodegenEncoding> encoding) {
        this.schema = schema;
        this.encoding = encoding;
    }

    public CodegenProperty getSchema() {
        return schema;
    }

    public LinkedHashMap<String, CodegenEncoding> getEncoding() {
        return encoding;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenMediaType{");
        sb.append("schema=").append(schema);
        sb.append(", encoding=").append(encoding);
        sb.append('}');
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenMediaType that = (CodegenMediaType) o;
        return Objects.equals(schema,that.getSchema()) &&
                Objects.equals(encoding, that.getEncoding());
    }

    @Override
    public int hashCode() {
        return Objects.hash(schema, encoding);
    }
}


