package io.swagger.client.model;

import io.swagger.client.model.FacetValue;
import java.util.*;

import com.wordnik.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;


@ApiModel(description = "")
public class Facet  {
  
  private List<FacetValue> facetValues = new ArrayList<FacetValue>() ;
  private String name = null;

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("facetValues")
  public List<FacetValue> getFacetValues() {
    return facetValues;
  }
  public void setFacetValues(List<FacetValue> facetValues) {
    this.facetValues = facetValues;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Facet {\n");
    
    sb.append("  facetValues: ").append(facetValues).append("\n");
    sb.append("  name: ").append(name).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
