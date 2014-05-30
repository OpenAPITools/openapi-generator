package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
public class WordList {
  @JsonProperty("id")
  private Long id = null;
  @JsonProperty("permalink")
  private String permalink = null;
  @JsonProperty("name")
  private String name = null;
  @JsonProperty("createdAt")
  private Date createdAt = null;
  @JsonProperty("updatedAt")
  private Date updatedAt = null;
  @JsonProperty("lastActivityAt")
  private Date lastActivityAt = null;
  @JsonProperty("username")
  private String username = null;
  @JsonProperty("userId")
  private Long userId = null;
  @JsonProperty("description")
  private String description = null;
  @JsonProperty("numberWordsInList")
  private Long numberWordsInList = null;
  @JsonProperty("type")
  private String type = null;
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public String getPermalink() {
    return permalink;
  }
  public void setPermalink(String permalink) {
    this.permalink = permalink;
  }

  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public Date getUpdatedAt() {
    return updatedAt;
  }
  public void setUpdatedAt(Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public Date getLastActivityAt() {
    return lastActivityAt;
  }
  public void setLastActivityAt(Date lastActivityAt) {
    this.lastActivityAt = lastActivityAt;
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

  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  public Long getNumberWordsInList() {
    return numberWordsInList;
  }
  public void setNumberWordsInList(Long numberWordsInList) {
    this.numberWordsInList = numberWordsInList;
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
    sb.append("class WordList {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  permalink: ").append(permalink).append("\n");
    sb.append("  name: ").append(name).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  updatedAt: ").append(updatedAt).append("\n");
    sb.append("  lastActivityAt: ").append(lastActivityAt).append("\n");
    sb.append("  username: ").append(username).append("\n");
    sb.append("  userId: ").append(userId).append("\n");
    sb.append("  description: ").append(description).append("\n");
    sb.append("  numberWordsInList: ").append(numberWordsInList).append("\n");
    sb.append("  type: ").append(type).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

