package com.wordnik.codegen.resource;

import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.annotate.JsonPropertyOrder;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import javax.annotation.Generated;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
@JsonPropertyOrder({
    "apiVersion",
    "swagrVersion",
    "basePath",
    "models",
    "id"
})
public class ApiResource implements Serializable
{

    @JsonProperty("apiVersion")
    private String apiVersion;
    @JsonProperty("swagrVersion")
    private String swagrVersion;
    @JsonProperty("basePath")
    private String basePath;
    @JsonProperty("models")
    private ApiModelListWrapper models;
    @JsonProperty("apis")
    private List<Endpoint> endPoints;
    @JsonProperty("id")
    private Object id;
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonCreator
    public ApiResource(@JsonProperty("models") ApiModelListWrapper models, @JsonProperty("apis") List<Endpoint> endPoints)
    {
      this.models = models;
      this.endPoints = endPoints;
    }

    @JsonProperty("apiVersion")
    public String getApiVersion() {
        return apiVersion;
    }

    @JsonProperty("apiVersion")
    public void setApiVersion(String apiVersion) {
        this.apiVersion = apiVersion;
    }

    @JsonProperty("swagrVersion")
    public String getSwagrVersion() {
        return swagrVersion;
    }

    @JsonProperty("swagrVersion")
    public void setSwagrVersion(String swagrVersion) {
        this.swagrVersion = swagrVersion;
    }

    @JsonProperty("basePath")
    public String getBasePath() {
        return basePath;
    }

    @JsonProperty("basePath")
    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    @JsonProperty("models")
    public ApiModelListWrapper getModels() {
        return models;
    }

    @JsonProperty("models")
    public void setModels(ApiModelListWrapper models) {
        this.models = models;
    }

    @JsonProperty("apis")
	public List<Endpoint> getEndPoints() {
		return endPoints;
	}

    @JsonProperty("apis")
	public void setEndPoints(List<Endpoint> endPoints) {
		this.endPoints = endPoints;
	}

    @JsonProperty("id")
    public Object getId() {
        return id;
    }

    @JsonProperty("id")
    public void setId(Object id) {
        this.id = id;
    }

}
