package com.wordnik.client.model;

import java.util.*;
import com.wordnik.client.model.FacetValue;
public class Facet {
  private List<FacetValue> facetValues = new ArrayList<FacetValue>();
  private String name = null;
  public List<FacetValue> getFacetValues() {
    return facetValues;
  }
  public void setFacetValues(List<FacetValue> facetValues) {
    this.facetValues = facetValues;
  }

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

