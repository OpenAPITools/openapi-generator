package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SimpleDefinition {
  @JsonProperty("text")
  private String text = null;
  @JsonProperty("source")
  private String source = null;
  @JsonProperty("note")
  private String note = null;
  @JsonProperty("partOfSpeech")
  private String partOfSpeech = null;
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  public String getSource() {
    return source;
  }
  public void setSource(String source) {
    this.source = source;
  }

  public String getNote() {
    return note;
  }
  public void setNote(String note) {
    this.note = note;
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
    sb.append("class SimpleDefinition {\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  source: ").append(source).append("\n");
    sb.append("  note: ").append(note).append("\n");
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

