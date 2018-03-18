package org.openapitools.codegen;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import java.util.LinkedHashMap;
import java.util.Map;

public class CliOption {
    private final String opt;
    private String description;
    private String type;
    private String defaultValue;
    private Map<String, String> enumValues;

    public CliOption(String opt, String description) {
        this(opt, description, SchemaTypeUtil.STRING_TYPE);
    }

    public CliOption(String opt, String description, String type) {
        this.opt = opt;
        this.description = description;
        this.type = type;
    }

    public String getOpt() {
        return opt;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getDefault() {
        return defaultValue;
    }

    public void setDefault(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public CliOption defaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
        return this;
    }

    public CliOption addEnum(String value, String description) {
        if (this.enumValues == null) {
            this.enumValues = new LinkedHashMap<String, String>();
        }
        if (!enumValues.containsKey(value)) {
            enumValues.put(value, description);
        }
        return this;
    }

    public Map<String, String> getEnum() {
        return enumValues;
    }

    public void setEnum(Map<String, String> enumValues) {
        this.enumValues = enumValues;
    }

    public static CliOption newBoolean(String opt, String description) {
        return new CliOption(opt, description, SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString());
    }

    public static CliOption newString(String opt, String description) {
        return new CliOption(opt, description, SchemaTypeUtil.STRING_TYPE);
    }

    @JsonIgnore
    public String getOptionHelp() {
        StringBuilder sb = new StringBuilder(description);
        if(defaultValue != null) {
            sb.append(" (Default: ").append(defaultValue).append(")");
        }
        if (enumValues != null) {
            for (Map.Entry<String, String> entry : enumValues.entrySet()) {
                sb.append("\n    ").append(entry.getKey()).append(" - ").append(entry.getValue());
            }
        }
        return sb.toString();
    }
}
