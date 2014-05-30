package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.*;
import com.wordnik.client.model.Definition;
public class DefinitionSearchResults {
  @JsonProperty("results")
  private List<Definition> results = new ArrayList<Definition>();
  @JsonProperty("totalResults")
  private Integer totalResults = null;
  public List<Definition> getResults() {
    return results;
  }
  public void setResults(List<Definition> results) {
    this.results = results;
  }

  public Integer getTotalResults() {
    return totalResults;
  }
  public void setTotalResults(Integer totalResults) {
    this.totalResults = totalResults;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class DefinitionSearchResults {\n");
    sb.append("  results: ").append(results).append("\n");
    sb.append("  totalResults: ").append(totalResults).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

