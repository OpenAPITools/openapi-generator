package com.wordnik.client.model;

import java.util.Date;
public class WordList {
  private Date updatedAt = null;
  private Long id = null;
  private String username = null;
  private String permalink = null;
  private Date lastActivityAt = null;
  private Date createdAt = null;
  private String description = null;
  private Long userId = null;
  private String name = null;
  private Long numberWordsInList = null;
  private String type = null;
  public Date getUpdatedAt() {
    return updatedAt;
  }
  public void setUpdatedAt(Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  public String getPermalink() {
    return permalink;
  }
  public void setPermalink(String permalink) {
    this.permalink = permalink;
  }

  public Date getLastActivityAt() {
    return lastActivityAt;
  }
  public void setLastActivityAt(Date lastActivityAt) {
    this.lastActivityAt = lastActivityAt;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  public Long getUserId() {
    return userId;
  }
  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
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
    sb.append("  updatedAt: ").append(updatedAt).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  username: ").append(username).append("\n");
    sb.append("  permalink: ").append(permalink).append("\n");
    sb.append("  lastActivityAt: ").append(lastActivityAt).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  description: ").append(description).append("\n");
    sb.append("  userId: ").append(userId).append("\n");
    sb.append("  name: ").append(name).append("\n");
    sb.append("  numberWordsInList: ").append(numberWordsInList).append("\n");
    sb.append("  type: ").append(type).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

