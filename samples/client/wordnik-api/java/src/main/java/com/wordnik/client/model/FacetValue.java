package com.wordnik.client.model;

public class FacetValue {
  private Long count = null;
  private String value = null;
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  public String getValue() {
    return value;
  }
  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class FacetValue {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  value: ").append(value).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

