package com.wordnik.client.model;

public class ExampleUsage {
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

