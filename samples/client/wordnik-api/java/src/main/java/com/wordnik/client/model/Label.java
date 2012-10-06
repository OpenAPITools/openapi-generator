package com.wordnik.client.model;

public class Label {
  private String text = null;
  private String type = null;
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Label {\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  type: ").append(type).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

