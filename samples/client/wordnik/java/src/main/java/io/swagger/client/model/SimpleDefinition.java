package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class SimpleDefinition {

    private String text = null;
    private String source = null;
    private String note = null;
    private String partOfSpeech = null;


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
    @JsonProperty("source")
    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("note")
    public String getNote() {
        return note;
    }

    public void setNote(String note) {
        this.note = note;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("partOfSpeech")
    public String getPartOfSpeech() {
        return partOfSpeech;
    }

    public void setPartOfSpeech(String partOfSpeech) {
        this.partOfSpeech = partOfSpeech;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class SimpleDefinition {\n");

        sb.append("  text: ").append(text).append("\n");
        sb.append("  source: ").append(source).append("\n");
        sb.append("  note: ").append(note).append("\n");
        sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
