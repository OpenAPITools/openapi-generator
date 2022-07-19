package org.openapitools.codegen.option;

import java.util.List;
import java.util.Set;
import java.util.HashSet;

public class CustomOptionDefinition {
    private String name;
    private String category;
    private String baseType;
    private boolean isEnum;
    private Set<String> allowedValues;

    public CustomOptionDefinition (String name, String category, String baseType) {
        this.name = name;
        this.category = category;
        this.baseType = baseType;
        this.isEnum = false;
    }

    public void setEnum(List<String> values) {
        this.isEnum = true;
        allowedValues = new HashSet<>(values);
    }

    public String getName() {
        return this.name;
    }
    public String getCategory() {
        return this.category;
    }
    public String getBaseType() {
        return this.baseType;
    }
    public boolean getIsEnum() {
        return this.isEnum;
    }
    public Set<String> getAllowedValues() {
        return this.allowedValues;
    }
}