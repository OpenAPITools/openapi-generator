package com.wordnik.client.model;

import java.util.*;
import com.wordnik.client.model.Facet;
import com.wordnik.client.model.Example;
public class ExampleSearchResults {
  private List<Facet> facets = new ArrayList<Facet>();
  private List<Example> examples = new ArrayList<Example>();
  public List<Facet> getFacets() {
    return facets;
  }
  public void setFacets(List<Facet> facets) {
    this.facets = facets;
  }

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

