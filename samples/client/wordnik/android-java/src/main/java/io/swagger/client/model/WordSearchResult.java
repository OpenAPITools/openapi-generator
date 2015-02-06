package io.swagger.client.model;


import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class WordSearchResult  { 
  private Long count = null;
  
  //public enum countEnum {  }; 
  
  private Double lexicality = null;
  
  //public enum lexicalityEnum {  }; 
  
  private String word = null;
  
  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getCount() {
    return count;
  }
  public void setCount(Long count) {
    this.count = count;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Double getLexicality() {
    return lexicality;
  }
  public void setLexicality(Double lexicality) {
    this.lexicality = lexicality;
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

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordSearchResult {\n");
    
    sb.append("  count: ").append(count).append("\n");
    sb.append("  lexicality: ").append(lexicality).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
