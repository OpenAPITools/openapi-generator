package com.wordnik.client.model;

public class Citation {
  private String cite = null;
  private String source = null;
  public String getCite() {
    return cite;
  }
  public void setCite(String cite) {
    this.cite = cite;
  }

  public String getSource() {
    return source;
  }
  public void setSource(String source) {
    this.source = source;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Citation {\n");
    sb.append("  cite: ").append(cite).append("\n");
    sb.append("  source: ").append(source).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

