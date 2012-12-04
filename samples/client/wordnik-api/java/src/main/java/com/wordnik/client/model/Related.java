package com.wordnik.client.model;

import java.util.*;
public class Related {
  private String label1 = null;
  private String relationshipType = null;
  private String label2 = null;
  private String label3 = null;
  private List<String> words = new ArrayList<String>();
  private String gram = null;
  private String label4 = null;
  public String getLabel1() {
    return label1;
  }
  public void setLabel1(String label1) {
    this.label1 = label1;
  }

  public String getRelationshipType() {
    return relationshipType;
  }
  public void setRelationshipType(String relationshipType) {
    this.relationshipType = relationshipType;
  }

  public String getLabel2() {
    return label2;
  }
  public void setLabel2(String label2) {
    this.label2 = label2;
  }

  public String getLabel3() {
    return label3;
  }
  public void setLabel3(String label3) {
    this.label3 = label3;
  }

  public List<String> getWords() {
    return words;
  }
  public void setWords(List<String> words) {
    this.words = words;
  }

  public String getGram() {
    return gram;
  }
  public void setGram(String gram) {
    this.gram = gram;
  }

  public String getLabel4() {
    return label4;
  }
  public void setLabel4(String label4) {
    this.label4 = label4;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Related {\n");
    sb.append("  label1: ").append(label1).append("\n");
    sb.append("  relationshipType: ").append(relationshipType).append("\n");
    sb.append("  label2: ").append(label2).append("\n");
    sb.append("  label3: ").append(label3).append("\n");
    sb.append("  words: ").append(words).append("\n");
    sb.append("  gram: ").append(gram).append("\n");
    sb.append("  label4: ").append(label4).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

