package com.wordnik.swagger.codegen.resource;

import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
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
