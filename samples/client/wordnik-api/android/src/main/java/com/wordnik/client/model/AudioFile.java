package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
public class AudioFile {
  @JsonProperty("attributionUrl")
  private String attributionUrl = null;
  @JsonProperty("commentCount")
  private Integer commentCount = null;
  @JsonProperty("voteCount")
  private Integer voteCount = null;
  @JsonProperty("fileUrl")
  private String fileUrl = null;
  @JsonProperty("audioType")
  private String audioType = null;
  @JsonProperty("id")
  private Long id = null;
  @JsonProperty("duration")
  private Double duration = null;
  @JsonProperty("attributionText")
  private String attributionText = null;
  @JsonProperty("createdBy")
  private String createdBy = null;
  @JsonProperty("description")
  private String description = null;
  @JsonProperty("createdAt")
  private Date createdAt = null;
  @JsonProperty("voteWeightedAverage")
  private Float voteWeightedAverage = null;
  @JsonProperty("voteAverage")
  private Float voteAverage = null;
  @JsonProperty("word")
  private String word = null;
  public String getAttributionUrl() {
    return attributionUrl;
  }
  public void setAttributionUrl(String attributionUrl) {
    this.attributionUrl = attributionUrl;
  }

  public Integer getCommentCount() {
    return commentCount;
  }
  public void setCommentCount(Integer commentCount) {
    this.commentCount = commentCount;
  }

  public Integer getVoteCount() {
    return voteCount;
  }
  public void setVoteCount(Integer voteCount) {
    this.voteCount = voteCount;
  }

  public String getFileUrl() {
    return fileUrl;
  }
  public void setFileUrl(String fileUrl) {
    this.fileUrl = fileUrl;
  }

  public String getAudioType() {
    return audioType;
  }
  public void setAudioType(String audioType) {
    this.audioType = audioType;
  }

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public Double getDuration() {
    return duration;
  }
  public void setDuration(Double duration) {
    this.duration = duration;
  }

  public String getAttributionText() {
    return attributionText;
  }
  public void setAttributionText(String attributionText) {
    this.attributionText = attributionText;
  }

  public String getCreatedBy() {
    return createdBy;
  }
  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public Float getVoteWeightedAverage() {
    return voteWeightedAverage;
  }
  public void setVoteWeightedAverage(Float voteWeightedAverage) {
    this.voteWeightedAverage = voteWeightedAverage;
  }

  public Float getVoteAverage() {
    return voteAverage;
  }
  public void setVoteAverage(Float voteAverage) {
    this.voteAverage = voteAverage;
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
    sb.append("class AudioFile {\n");
    sb.append("  attributionUrl: ").append(attributionUrl).append("\n");
    sb.append("  commentCount: ").append(commentCount).append("\n");
    sb.append("  voteCount: ").append(voteCount).append("\n");
    sb.append("  fileUrl: ").append(fileUrl).append("\n");
    sb.append("  audioType: ").append(audioType).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  duration: ").append(duration).append("\n");
    sb.append("  attributionText: ").append(attributionText).append("\n");
    sb.append("  createdBy: ").append(createdBy).append("\n");
    sb.append("  description: ").append(description).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  voteWeightedAverage: ").append(voteWeightedAverage).append("\n");
    sb.append("  voteAverage: ").append(voteAverage).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

