package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class WordSearchResult {

    private Long count = null;
    private Double lexicality = null;
    private String word = null;


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
    @JsonProperty("lexicality")
    public Double getLexicality() {
        return lexicality;
    }

    public void setLexicality(Double lexicality) {
        this.lexicality = lexicality;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("word")
    public String getWord() {
        return word;
    }

    public void setWord(String word) {
        this.word = word;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class WordSearchResult {\n");

        sb.append("  count: ").append(count).append("\n");
        sb.append("  lexicality: ").append(lexicality).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
