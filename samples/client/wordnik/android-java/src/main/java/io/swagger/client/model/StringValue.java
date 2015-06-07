package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class StringValue {

    private String word = null;


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
        sb.append("class StringValue {\n");

        sb.append("  word: ").append(word).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
