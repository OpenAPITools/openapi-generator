package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class ExampleUsage {

    private String text = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("text")
    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ExampleUsage {\n");

        sb.append("  text: ").append(text).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
