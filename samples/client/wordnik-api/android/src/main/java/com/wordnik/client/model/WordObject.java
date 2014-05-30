package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.*;
public class WordObject {
  @JsonProperty("id")
  private Long id = null;
  @JsonProperty("word")
  private String word = null;
  @JsonProperty("originalWord")
  private String originalWord = null;
  @JsonProperty("suggestions")
  private List<String> suggestions = new ArrayList<String>();
  @JsonProperty("canonicalForm")
  private String canonicalForm = null;
  @JsonProperty("vulgar")
  private String vulgar = null;
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public String getOriginalWord() {
    return originalWord;
  }
  public void setOriginalWord(String originalWord) {
    this.originalWord = originalWord;
  }

  public List<String> getSuggestions() {
    return suggestions;
  }
  public void setSuggestions(List<String> suggestions) {
    this.suggestions = suggestions;
  }

  public String getCanonicalForm() {
    return canonicalForm;
  }
  public void setCanonicalForm(String canonicalForm) {
    this.canonicalForm = canonicalForm;
  }

  public String getVulgar() {
    return vulgar;
  }
  public void setVulgar(String vulgar) {
    this.vulgar = vulgar;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordObject {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  originalWord: ").append(originalWord).append("\n");
    sb.append("  suggestions: ").append(suggestions).append("\n");
    sb.append("  canonicalForm: ").append(canonicalForm).append("\n");
    sb.append("  vulgar: ").append(vulgar).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

