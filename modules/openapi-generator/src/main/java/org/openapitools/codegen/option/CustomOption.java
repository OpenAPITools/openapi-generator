package org.openapitools.codegen.option;

public class CustomOption {
    private Object value;
    private CustomOptionDefinition definition;

    public CustomOption (CustomOptionDefinition definition, Object value) {
        this.definition = definition;
        this.value = value;
    }

    public CustomOptionDefinition getDefinition() {
        return this.definition;
    }
    public Object getValue() {
        return this.value;
    }
    public void setValue(Object val) {
        this.value = val;
    }
}