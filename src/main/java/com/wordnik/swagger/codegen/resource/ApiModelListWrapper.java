package com.wordnik.swagger.codegen.resource;

import org.codehaus.jackson.annotate.JsonAnyGetter;
import org.codehaus.jackson.annotate.JsonAnySetter;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class ApiModelListWrapper implements Serializable
{

    private Map<String, ApiModelDefn> modelList = new HashMap<String, ApiModelDefn>();

    @JsonAnyGetter
    public Map<String, ApiModelDefn> getModelList() {
        return this.modelList;
    }

    @JsonAnySetter
    public void setModelList(String modelName, ApiModelDefn modelDefn) {
        this.modelList.put(modelName, modelDefn);
    }

}
