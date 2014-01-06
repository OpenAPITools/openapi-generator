package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Frequency {
  @JsonProperty("count")
  private Long count = null;
  @JsonProperty("year")
  private Integer year = null;
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  public Integer getYear() {
    return year;
  }
  public void setYear(Integer year) {
    this.year = year;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Frequency {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  year: ").append(year).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

