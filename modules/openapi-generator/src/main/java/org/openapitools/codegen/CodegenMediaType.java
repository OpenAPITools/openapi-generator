package org.openapitools.codegen;

import io.swagger.v3.oas.models.examples.Example;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public class CodegenMediaType {
    private CodegenProperty schema;
    private LinkedHashMap<String, CodegenEncoding> encoding;
    private HashMap<String, SchemaTestCase> testCases = new HashMap<>();
    private Map<String, Example> examples = null;

    public CodegenMediaType(CodegenProperty schema, LinkedHashMap<String, CodegenEncoding> encoding, HashMap<String, SchemaTestCase> testCases) {
        this.schema = schema;
        this.encoding = encoding;
        if (testCases != null) {
            this.testCases = testCases;
        }
    }

    public CodegenMediaType(CodegenProperty schema, LinkedHashMap<String, CodegenEncoding> encoding, HashMap<String, SchemaTestCase> testCases, Map<String, Example> examples) {
        this(schema, encoding, testCases);
        this.examples = examples;
    }

    public CodegenProperty getSchema() {
        return schema;
    }

    public LinkedHashMap<String, CodegenEncoding> getEncoding() {
        return encoding;
    }

    public HashMap<String, SchemaTestCase> getTestCases() { return testCases; }

    public Map<String, Example> getExamples() {
        return examples;
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


