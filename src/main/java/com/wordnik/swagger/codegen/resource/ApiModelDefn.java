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

import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import org.codehaus.jackson.annotate.JsonProperty;

/**
 * User: deepakmichael
 * Date: 19/07/11
 * Time: 1:21 AM
 */
public class ApiModelDefn {

    @JsonProperty("id")
    private String id;
    @JsonProperty("properties")
    private ApiPropertyListWrapper properties;
    @JsonProperty("description")
    private String description;

    @JsonProperty("id")
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @JsonProperty("properties")
    public ApiPropertyListWrapper getProperties() {
        return properties;
    }

    public void setProperties(ApiPropertyListWrapper properties) {
        this.properties = properties;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Model toModel(String modelName, NamingPolicyProvider nameGenerator) {
        Model model = new Model();
        model.setName(modelName);
        model.setDescription(this.getDescription());
        model.setFields( this.getProperties().toFieldList( nameGenerator ) );
        return model;
    }
}
