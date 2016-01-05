package io.swagger.codegen;

import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.StringProperty;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.LinkedHashMap;
import java.util.Map;

public class CliOption {
    private final String opt;
    private String description;
    private String type;
    private String defaultValue;
    private Map<String, String> enumValues;

    public CliOption(String opt, String description) {
        this(opt, description, StringProperty.TYPE);
    }

    public CliOption(String opt, String description, String type) {
        this.opt = opt;
        this.description = description;
        this.type = type;
    }

    @ApiModelProperty(name = "optionName")
    public String getOpt() {
        return opt;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @ApiModelProperty(value = "Data type is based on the types supported by the JSON-Schema")
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
        return new CliOption(opt, description, BooleanProperty.TYPE).defaultValue(Boolean.FALSE.toString());
    }

    public static CliOption newString(String opt, String description) {
        return new CliOption(opt, description, StringProperty.TYPE);
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
