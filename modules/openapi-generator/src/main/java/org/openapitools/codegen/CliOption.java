/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public class CliOption {
    @Getter private final String opt;
    @Getter @Setter
    private String description;
    @Getter @Setter
    private String type;
    private String defaultValue;
    @Getter private String optValue;
    private Map<String, String> enumValues;

    public CliOption(String opt, String description) {
        this(opt, description, SchemaTypeUtil.STRING_TYPE);
    }

    public CliOption(String opt, String description, String type) {
        this.opt = opt;
        this.description = description;
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

    public void setOptValue(String optValue) {
        if (this.enumValues != null && this.enumValues.containsKey(optValue)) {
            this.optValue = optValue;
        } else {
            this.optValue = null;
        }
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

    /**
     * Create new boolean command line option with a default of false
     *
     * @param opt         Option name
     * @param description Option description
     * @return the CliOption created
     */
    public static CliOption newBoolean(String opt, String description) {
        return newBoolean(opt, description, false);
    }

    /**
     * Create new boolean command line option with the provided value as default
     *
     * @param opt          Option name
     * @param description  Option description
     * @param defaultValue the default value to use if option not specified
     * @return the CliOption created
     */
    public static CliOption newBoolean(String opt, String description, boolean defaultValue) {
        return new CliOption(opt, description, SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(String.valueOf(defaultValue));
    }

    public static CliOption newString(String opt, String description) {
        return new CliOption(opt, description, SchemaTypeUtil.STRING_TYPE);
    }

    @JsonIgnore
    public String getOptionHelp() {
        StringBuilder sb = new StringBuilder(description);
        if (defaultValue != null) {
            sb.append(" (Default: ").append(defaultValue).append(")");
        }
        if (enumValues != null) {
            for (Map.Entry<String, String> entry : enumValues.entrySet()) {
                sb.append("\n    ").append(entry.getKey()).append(" - ").append(entry.getValue());
            }
        }
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CliOption cliOption = (CliOption) o;
        return Objects.equals(opt, cliOption.opt) &&
                Objects.equals(description, cliOption.description) &&
                Objects.equals(type, cliOption.type) &&
                Objects.equals(defaultValue, cliOption.defaultValue) &&
                Objects.equals(optValue, cliOption.optValue) &&
                Objects.equals(enumValues, cliOption.enumValues);
    }

    @Override
    public int hashCode() {
        return Objects.hash(opt, description, type, defaultValue, optValue, enumValues);
    }
}
