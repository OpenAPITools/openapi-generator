package com.wordnik.client.model;

public class Bigram {
  private Long count = null;
  private String gram2 = null;
  private String gram1 = null;
  private Double wlmi = null;
  private Double mi = null;
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  public String getGram2() {
    return gram2;
  }
  public void setGram2(String gram2) {
    this.gram2 = gram2;
  }

  public String getGram1() {
    return gram1;
  }
  public void setGram1(String gram1) {
    this.gram1 = gram1;
  }

  public Double getWlmi() {
    return wlmi;
  }
  public void setWlmi(Double wlmi) {
    this.wlmi = wlmi;
  }

  public Double getMi() {
    return mi;
  }
  public void setMi(Double mi) {
    this.mi = mi;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Bigram {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  gram2: ").append(gram2).append("\n");
    sb.append("  gram1: ").append(gram1).append("\n");
    sb.append("  wlmi: ").append(wlmi).append("\n");
    sb.append("  mi: ").append(mi).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

