package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ScoredWord {
  @JsonProperty("position")
  private Integer position = null;
  @JsonProperty("id")
  private Long id = null;
  @JsonProperty("docTermCount")
  private Integer docTermCount = null;
  @JsonProperty("lemma")
  private String lemma = null;
  @JsonProperty("wordType")
  private String wordType = null;
  @JsonProperty("score")
  private Float score = null;
  @JsonProperty("sentenceId")
  private Long sentenceId = null;
  @JsonProperty("word")
  private String word = null;
  @JsonProperty("stopword")
  private Boolean stopword = null;
  @JsonProperty("baseWordScore")
  private Double baseWordScore = null;
  @JsonProperty("partOfSpeech")
  private String partOfSpeech = null;
  public Integer getPosition() {
    return position;
  }
  public void setPosition(Integer position) {
    this.position = position;
  }

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public Integer getDocTermCount() {
    return docTermCount;
  }
  public void setDocTermCount(Integer docTermCount) {
    this.docTermCount = docTermCount;
  }

  public String getLemma() {
    return lemma;
  }
  public void setLemma(String lemma) {
    this.lemma = lemma;
  }

  public String getWordType() {
    return wordType;
  }
  public void setWordType(String wordType) {
    this.wordType = wordType;
  }

  public Float getScore() {
    return score;
  }
  public void setScore(Float score) {
    this.score = score;
  }

  public Long getSentenceId() {
    return sentenceId;
  }
  public void setSentenceId(Long sentenceId) {
    this.sentenceId = sentenceId;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public Boolean getStopword() {
    return stopword;
  }
  public void setStopword(Boolean stopword) {
    this.stopword = stopword;
  }

  public Double getBaseWordScore() {
    return baseWordScore;
  }
  public void setBaseWordScore(Double baseWordScore) {
    this.baseWordScore = baseWordScore;
  }

  public String getPartOfSpeech() {
    return partOfSpeech;
  }
  public void setPartOfSpeech(String partOfSpeech) {
    this.partOfSpeech = partOfSpeech;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ScoredWord {\n");
    sb.append("  position: ").append(position).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  docTermCount: ").append(docTermCount).append("\n");
    sb.append("  lemma: ").append(lemma).append("\n");
    sb.append("  wordType: ").append(wordType).append("\n");
    sb.append("  score: ").append(score).append("\n");
    sb.append("  sentenceId: ").append(sentenceId).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  stopword: ").append(stopword).append("\n");
    sb.append("  baseWordScore: ").append(baseWordScore).append("\n");
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

