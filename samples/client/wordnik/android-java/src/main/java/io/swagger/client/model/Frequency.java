package io.swagger.client.model;


import com.wordnik.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;


@ApiModel(description = "")
public class Frequency  {
  
  private Long count = null;
  private Integer year = null;

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("count")
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  @JsonProperty("year")
  public Integer getYear() {
    return year;
  }
  public void setYear(Integer year) {
    this.year = year;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Frequency {\n");
    
    sb.append("  count: ").append(count).append("\n");
    sb.append("  year: ").append(year).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
