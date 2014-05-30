package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ExampleUsage {
  @JsonProperty("text")
  private String text = null;
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ExampleUsage {\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

