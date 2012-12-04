package com.wordnik.client.model;

public class ScoredWord {
  private Integer position = null;
  private Long id = null;
  private Integer docTermCount = null;
  private String lemma = null;
  private String wordType = null;
  private Float score = null;
  private Long sentenceId = null;
  private String word = null;
  private Boolean stopword = null;
  private Double baseWordScore = null;
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

