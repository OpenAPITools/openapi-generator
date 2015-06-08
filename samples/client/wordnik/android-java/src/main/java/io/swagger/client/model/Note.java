package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class Note {

    private String noteType = null;
    private List<String> appliesTo = new ArrayList<String>();
    private String value = null;
    private Integer pos = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("noteType")
    public String getNoteType() {
        return noteType;
    }

    public void setNoteType(String noteType) {
        this.noteType = noteType;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("appliesTo")
    public List<String> getAppliesTo() {
        return appliesTo;
    }

    public void setAppliesTo(List<String> appliesTo) {
        this.appliesTo = appliesTo;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("value")
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("pos")
    public Integer getPos() {
        return pos;
    }

    public void setPos(Integer pos) {
        this.pos = pos;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Note {\n");

        sb.append("  noteType: ").append(noteType).append("\n");
        sb.append("  appliesTo: ").append(appliesTo).append("\n");
        sb.append("  value: ").append(value).append("\n");
        sb.append("  pos: ").append(pos).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
