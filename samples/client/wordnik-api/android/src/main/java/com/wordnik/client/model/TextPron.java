package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TextPron {
  @JsonProperty("raw")
  private String raw = null;
  @JsonProperty("seq")
  private Integer seq = null;
  @JsonProperty("rawType")
  private String rawType = null;
  public String getRaw() {
    return raw;
  }
  public void setRaw(String raw) {
    this.raw = raw;
  }

  public Integer getSeq() {
    return seq;
  }
  public void setSeq(Integer seq) {
    this.seq = seq;
  }

  public String getRawType() {
    return rawType;
  }
  public void setRawType(String rawType) {
    this.rawType = rawType;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class TextPron {\n");
    sb.append("  raw: ").append(raw).append("\n");
    sb.append("  seq: ").append(seq).append("\n");
    sb.append("  rawType: ").append(rawType).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

