package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class WordSearchResult {
  @JsonProperty("count")
  private Long count = null;
  @JsonProperty("lexicality")
  private Double lexicality = null;
  @JsonProperty("word")
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

