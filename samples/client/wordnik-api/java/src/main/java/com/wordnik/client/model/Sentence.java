package com.wordnik.client.model;

import java.util.*;
import com.wordnik.client.model.ScoredWord;
public class Sentence {
  private Boolean hasScoredWords = null;
  private Long id = null;
  private List<ScoredWord> scoredWords = new ArrayList<ScoredWord>();
  private String display = null;
  private Integer rating = null;
  private Long documentMetadataId = null;
  public Boolean getHasScoredWords() {
    return hasScoredWords;
  }
  public void setHasScoredWords(Boolean hasScoredWords) {
    this.hasScoredWords = hasScoredWords;
  }

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public List<ScoredWord> getScoredWords() {
    return scoredWords;
  }
  public void setScoredWords(List<ScoredWord> scoredWords) {
    this.scoredWords = scoredWords;
  }

  public String getDisplay() {
    return display;
  }
  public void setDisplay(String display) {
    this.display = display;
  }

  public Integer getRating() {
    return rating;
  }
  public void setRating(Integer rating) {
    this.rating = rating;
  }

  public Long getDocumentMetadataId() {
    return documentMetadataId;
  }
  public void setDocumentMetadataId(Long documentMetadataId) {
    this.documentMetadataId = documentMetadataId;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Sentence {\n");
    sb.append("  hasScoredWords: ").append(hasScoredWords).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  scoredWords: ").append(scoredWords).append("\n");
    sb.append("  display: ").append(display).append("\n");
    sb.append("  rating: ").append(rating).append("\n");
    sb.append("  documentMetadataId: ").append(documentMetadataId).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

