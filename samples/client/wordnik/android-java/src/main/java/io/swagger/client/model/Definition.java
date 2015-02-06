package io.swagger.client.model;

import io.swagger.client.model.ExampleUsage;
import io.swagger.client.model.Label;
import io.swagger.client.model.Note;
import java.util.*;
import io.swagger.client.model.Related;
import io.swagger.client.model.Citation;
import io.swagger.client.model.TextPron;

import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Definition  { 
  private String extendedText = null;
  private String text = null;
  private String sourceDictionary = null;
  private List<Citation> citations = new ArrayList<Citation>() ;
  private List<Label> labels = new ArrayList<Label>() ;
  private Float score = null;
  
  //public enum scoreEnum {  }; 
  
  private List<ExampleUsage> exampleUses = new ArrayList<ExampleUsage>() ;
  private String attributionUrl = null;
  private String seqString = null;
  private String attributionText = null;
  private List<Related> relatedWords = new ArrayList<Related>() ;
  private String sequence = null;
  private String word = null;
  private List<Note> notes = new ArrayList<Note>() ;
  private List<TextPron> textProns = new ArrayList<TextPron>() ;
  private String partOfSpeech = null;
  
  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getExtendedText() {
    return extendedText;
  }
  public void setExtendedText(String extendedText) {
    this.extendedText = extendedText;
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
  public String getSourceDictionary() {
    return sourceDictionary;
  }
  public void setSourceDictionary(String sourceDictionary) {
    this.sourceDictionary = sourceDictionary;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public List<Citation> getCitations() {
    return citations;
  }
  public void setCitations(List<Citation> citations) {
    this.citations = citations;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public List<Label> getLabels() {
    return labels;
  }
  public void setLabels(List<Label> labels) {
    this.labels = labels;
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
  public List<ExampleUsage> getExampleUses() {
    return exampleUses;
  }
  public void setExampleUses(List<ExampleUsage> exampleUses) {
    this.exampleUses = exampleUses;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getAttributionUrl() {
    return attributionUrl;
  }
  public void setAttributionUrl(String attributionUrl) {
    this.attributionUrl = attributionUrl;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getSeqString() {
    return seqString;
  }
  public void setSeqString(String seqString) {
    this.seqString = seqString;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getAttributionText() {
    return attributionText;
  }
  public void setAttributionText(String attributionText) {
    this.attributionText = attributionText;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public List<Related> getRelatedWords() {
    return relatedWords;
  }
  public void setRelatedWords(List<Related> relatedWords) {
    this.relatedWords = relatedWords;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getSequence() {
    return sequence;
  }
  public void setSequence(String sequence) {
    this.sequence = sequence;
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
  public List<Note> getNotes() {
    return notes;
  }
  public void setNotes(List<Note> notes) {
    this.notes = notes;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public List<TextPron> getTextProns() {
    return textProns;
  }
  public void setTextProns(List<TextPron> textProns) {
    this.textProns = textProns;
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
    sb.append("class Definition {\n");
    
    sb.append("  extendedText: ").append(extendedText).append("\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  sourceDictionary: ").append(sourceDictionary).append("\n");
    sb.append("  citations: ").append(citations).append("\n");
    sb.append("  labels: ").append(labels).append("\n");
    sb.append("  score: ").append(score).append("\n");
    sb.append("  exampleUses: ").append(exampleUses).append("\n");
    sb.append("  attributionUrl: ").append(attributionUrl).append("\n");
    sb.append("  seqString: ").append(seqString).append("\n");
    sb.append("  attributionText: ").append(attributionText).append("\n");
    sb.append("  relatedWords: ").append(relatedWords).append("\n");
    sb.append("  sequence: ").append(sequence).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  notes: ").append(notes).append("\n");
    sb.append("  textProns: ").append(textProns).append("\n");
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
