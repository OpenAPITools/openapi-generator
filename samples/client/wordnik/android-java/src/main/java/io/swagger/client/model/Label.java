package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Label {

    private String text = null;
    private String type = null;


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


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("type")
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Label {\n");

        sb.append("  text: ").append(text).append("\n");
        sb.append("  type: ").append(type).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
