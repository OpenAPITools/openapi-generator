package com.wordnik.client.model;

public class StringValue {
  private String word = null;
  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class StringValue {\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

