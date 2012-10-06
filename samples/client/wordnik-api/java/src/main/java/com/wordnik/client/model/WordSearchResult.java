package com.wordnik.client.model;

public class WordSearchResult {
  private Long count = null;
  private Double lexicality = null;
  private String word = null;
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  public Double getLexicality() {
    return lexicality;
  }
  public void setLexicality(Double lexicality) {
    this.lexicality = lexicality;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordSearchResult {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  lexicality: ").append(lexicality).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

