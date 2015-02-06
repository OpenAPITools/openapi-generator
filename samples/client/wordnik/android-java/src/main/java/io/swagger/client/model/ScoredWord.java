package io.swagger.client.model;


import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class ScoredWord  { 
  private Integer position = null;
  
  //public enum positionEnum {  }; 
  
  private Long id = null;
  
  //public enum idEnum {  }; 
  
  private Integer docTermCount = null;
  
  //public enum docTermCountEnum {  }; 
  
  private String lemma = null;
  private String wordType = null;
  private Float score = null;
  
  //public enum scoreEnum {  }; 
  
  private Long sentenceId = null;
  
  //public enum sentenceIdEnum {  }; 
  
  private String word = null;
  private Boolean stopword = null;
  private Double baseWordScore = null;
  
  //public enum baseWordScoreEnum {  }; 
  
  private String partOfSpeech = null;
  
  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Integer getPosition() {
    return position;
  }
  public void setPosition(Integer position) {
    this.position = position;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Integer getDocTermCount() {
    return docTermCount;
  }
  public void setDocTermCount(Integer docTermCount) {
    this.docTermCount = docTermCount;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getLemma() {
    return lemma;
  }
  public void setLemma(String lemma) {
    this.lemma = lemma;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getWordType() {
    return wordType;
  }
  public void setWordType(String wordType) {
    this.wordType = wordType;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Float getScore() {
    return score;
  }
  public void setScore(Float score) {
    this.score = score;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getSentenceId() {
    return sentenceId;
  }
  public void setSentenceId(Long sentenceId) {
    this.sentenceId = sentenceId;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Boolean getStopword() {
    return stopword;
  }
  public void setStopword(Boolean stopword) {
    this.stopword = stopword;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Double getBaseWordScore() {
    return baseWordScore;
  }
  public void setBaseWordScore(Double baseWordScore) {
    this.baseWordScore = baseWordScore;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
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
