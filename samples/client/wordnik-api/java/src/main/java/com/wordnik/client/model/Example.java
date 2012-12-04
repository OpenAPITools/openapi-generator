package com.wordnik.client.model;

import com.wordnik.client.model.Sentence;
import com.wordnik.client.model.ScoredWord;
import com.wordnik.client.model.ContentProvider;
public class Example {
  private Long id = null;
  private Long exampleId = null;
  private String title = null;
  private String text = null;
  private ScoredWord score = null;
  private Sentence sentence = null;
  private String word = null;
  private ContentProvider provider = null;
  private Integer year = null;
  private Float rating = null;
  private Long documentId = null;
  private String url = null;
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public Long getExampleId() {
    return exampleId;
  }
  public void setExampleId(Long exampleId) {
    this.exampleId = exampleId;
  }

  public String getTitle() {
    return title;
  }
  public void setTitle(String title) {
    this.title = title;
  }

  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  public ScoredWord getScore() {
    return score;
  }
  public void setScore(ScoredWord score) {
    this.score = score;
  }

  public Sentence getSentence() {
    return sentence;
  }
  public void setSentence(Sentence sentence) {
    this.sentence = sentence;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public ContentProvider getProvider() {
    return provider;
  }
  public void setProvider(ContentProvider provider) {
    this.provider = provider;
  }

  public Integer getYear() {
    return year;
  }
  public void setYear(Integer year) {
    this.year = year;
  }

  public Float getRating() {
    return rating;
  }
  public void setRating(Float rating) {
    this.rating = rating;
  }

  public Long getDocumentId() {
    return documentId;
  }
  public void setDocumentId(Long documentId) {
    this.documentId = documentId;
  }

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

