package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ScrabbleScoreResult {
  @JsonProperty("value")
  private Integer value = null;
  public Integer getValue() {
    return value;
  }
  public void setValue(Integer value) {
    this.value = value;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ScrabbleScoreResult {\n");
    sb.append("  value: ").append(value).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

