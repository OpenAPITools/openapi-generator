package com.wordnik.client.model;

import java.util.*;
import com.wordnik.client.model.Frequency;
public class FrequencySummary {
  private Integer unknownYearCount = null;
  private Long totalCount = null;
  private String frequencyString = null;
  private String word = null;
  private List<Frequency> frequency = new ArrayList<Frequency>();
  public Integer getUnknownYearCount() {
    return unknownYearCount;
  }
  public void setUnknownYearCount(Integer unknownYearCount) {
    this.unknownYearCount = unknownYearCount;
  }

  public Long getTotalCount() {
    return totalCount;
  }
  public void setTotalCount(Long totalCount) {
    this.totalCount = totalCount;
  }

  public String getFrequencyString() {
    return frequencyString;
  }
  public void setFrequencyString(String frequencyString) {
    this.frequencyString = frequencyString;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public List<Frequency> getFrequency() {
    return frequency;
  }
  public void setFrequency(List<Frequency> frequency) {
    this.frequency = frequency;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class FrequencySummary {\n");
    sb.append("  unknownYearCount: ").append(unknownYearCount).append("\n");
    sb.append("  totalCount: ").append(totalCount).append("\n");
    sb.append("  frequencyString: ").append(frequencyString).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  frequency: ").append(frequency).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

