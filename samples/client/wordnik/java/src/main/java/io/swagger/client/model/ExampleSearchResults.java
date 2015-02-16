package io.swagger.client.model;

import io.swagger.client.model.Example;
import java.util.*;
import io.swagger.client.model.Facet;

import com.wordnik.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;


@ApiModel(description = "")
public class ExampleSearchResults  {
  
  private List<Facet> facets = new ArrayList<Facet>() ;
  private List<Example> examples = new ArrayList<Example>() ;

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("facets")
  public List<Facet> getFacets() {
    return facets;
  }
  public void setFacets(List<Facet> facets) {
    this.facets = facets;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("examples")
  public List<Example> getExamples() {
    return examples;
  }
  public void setExamples(List<Example> examples) {
    this.examples = examples;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ExampleSearchResults {\n");
    
    sb.append("  facets: ").append(facets).append("\n");
    sb.append("  examples: ").append(examples).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
