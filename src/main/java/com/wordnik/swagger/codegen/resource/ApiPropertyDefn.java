/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.resource;

import org.codehaus.jackson.annotate.JsonAnyGetter;
import org.codehaus.jackson.annotate.JsonAnySetter;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.annotate.JsonPropertyOrder;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
@JsonPropertyOrder({
    "id",
    "default",
    "items",
    "description",
    "name",
    "allowableValues",
    "properties",
    "required",
    "notes",
    "access",
    "type"
})
public class ApiPropertyDefn implements Serializable {

    @JsonProperty("id")
    private String id;
    @JsonProperty("default")
    private String defaultValue;
    @JsonProperty("items")
    private ApiPropertyDefn items;
    @JsonProperty("description")
    private String description;
    @JsonProperty("name")
    private String name;
    @JsonProperty("allowableValues")
    private AllowableValues allowableValues = null;
    @JsonProperty("properties")
    private ApiPropertyListWrapper properties;
    @JsonProperty("required")
    private boolean required;
    @JsonProperty("notes")
    private String notes;
    @JsonProperty("access")
    private String access;
    @JsonProperty("type")
    private String type;
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("id")
    public String getId() {
        return id;
    }

    @JsonProperty("id")
    public void setId(String id) {
        this.id = id;
    }

    @JsonProperty("default")
    public String getDefaultValue() {
        return defaultValue;
    }

    @JsonProperty("default")
    public void setDefault(String defaultvalue) {
        this.defaultValue = defaultValue;
    }

    @JsonProperty("items")
    public ApiPropertyDefn getItems() {
        return items;
    }

    @JsonProperty("items")
    public void setItems(ApiPropertyDefn items) {
        this.items = items;
    }

    @JsonProperty("description")
    public String getDescription() {
        return description;
    }

    @JsonProperty("description")
    public void setDescription(String description) {
        this.description = description;
    }

    @JsonProperty("name")
    public String getName() {
        return name;
    }

    @JsonProperty("name")
    public void setName(String name) {
        this.name = name;
    }

    @JsonProperty("allowableValues")
    public AllowableValues getAllowableValues() {
        return allowableValues;
    }

    @JsonProperty("allowableValues")
    public void setAllowableValues(AllowableValues possibleValues) {
        this.allowableValues = possibleValues;
    }

    @JsonProperty("properties")
    public ApiPropertyListWrapper getProperties() {
        return properties;
    }

    @JsonProperty("properties")
    public void setProperties(ApiPropertyListWrapper properties) {
        this.properties = properties;
    }

    @JsonProperty("required")
    public boolean isRequired() {
        return required;
    }

    @JsonProperty("required")
    public void setRequired(boolean required) {
        this.required = required;
    }

    @JsonProperty("notes")
    public String getNotes() {
        return notes;
    }

    @JsonProperty("notes")
    public void setNotes(String notes) {
        this.notes = notes;
    }

    @JsonProperty("access")
    public String getAccess() {
        return access;
    }

    @JsonProperty("access")
    public void setAccess(String access) {
        this.access = access;
    }

    @JsonProperty("type")
    public String getType() {
        return type;
    }

    @JsonProperty("type")
    public void setType(String type) {
        this.type = type;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperties(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}
