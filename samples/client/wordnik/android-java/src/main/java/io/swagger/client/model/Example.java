package io.swagger.client.model;

import io.swagger.client.model.Sentence;
import io.swagger.client.model.ContentProvider;
import io.swagger.client.model.ScoredWord;

import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Example  { 
  private Long id = null;
  
  //public enum idEnum {  }; 
  
  private Long exampleId = null;
  
  //public enum exampleIdEnum {  }; 
  
  private String title = null;
  private String text = null;
  private ScoredWord score = null;
  private Sentence sentence = null;
  private String word = null;
  private ContentProvider provider = null;
  private Integer year = null;
  
  //public enum yearEnum {  }; 
  
  private Float rating = null;
  
  //public enum ratingEnum {  }; 
  
  private Long documentId = null;
  
  //public enum documentIdEnum {  }; 
  
  private String url = null;
  
  
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
  public Long getExampleId() {
    return exampleId;
  }
  public void setExampleId(Long exampleId) {
    this.exampleId = exampleId;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getTitle() {
    return title;
  }
  public void setTitle(String title) {
    this.title = title;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public ScoredWord getScore() {
    return score;
  }
  public void setScore(ScoredWord score) {
    this.score = score;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Sentence getSentence() {
    return sentence;
  }
  public void setSentence(Sentence sentence) {
    this.sentence = sentence;
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
  public ContentProvider getProvider() {
    return provider;
  }
  public void setProvider(ContentProvider provider) {
    this.provider = provider;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Integer getYear() {
    return year;
  }
  public void setYear(Integer year) {
    this.year = year;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Float getRating() {
    return rating;
  }
  public void setRating(Float rating) {
    this.rating = rating;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getDocumentId() {
    return documentId;
  }
  public void setDocumentId(Long documentId) {
    this.documentId = documentId;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getUrl() {
    return url;
  }
  public void setUrl(String url) {
    this.url = url;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Example {\n");
    
    sb.append("  id: ").append(id).append("\n");
    sb.append("  exampleId: ").append(exampleId).append("\n");
    sb.append("  title: ").append(title).append("\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  score: ").append(score).append("\n");
    sb.append("  sentence: ").append(sentence).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  provider: ").append(provider).append("\n");
    sb.append("  year: ").append(year).append("\n");
    sb.append("  rating: ").append(rating).append("\n");
    sb.append("  documentId: ").append(documentId).append("\n");
    sb.append("  url: ").append(url).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
