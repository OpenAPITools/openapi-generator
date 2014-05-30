package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
public class WordListWord {
  @JsonProperty("id")
  private Long id = null;
  @JsonProperty("word")
  private String word = null;
  @JsonProperty("username")
  private String username = null;
  @JsonProperty("userId")
  private Long userId = null;
  @JsonProperty("createdAt")
  private Date createdAt = null;
  @JsonProperty("numberCommentsOnWord")
  private Long numberCommentsOnWord = null;
  @JsonProperty("numberLists")
  private Long numberLists = null;
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

  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  public Long getUserId() {
    return userId;
  }
  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public Long getNumberCommentsOnWord() {
    return numberCommentsOnWord;
  }
  public void setNumberCommentsOnWord(Long numberCommentsOnWord) {
    this.numberCommentsOnWord = numberCommentsOnWord;
  }

  public Long getNumberLists() {
    return numberLists;
  }
  public void setNumberLists(Long numberLists) {
    this.numberLists = numberLists;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordListWord {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  username: ").append(username).append("\n");
    sb.append("  userId: ").append(userId).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  numberCommentsOnWord: ").append(numberCommentsOnWord).append("\n");
    sb.append("  numberLists: ").append(numberLists).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

