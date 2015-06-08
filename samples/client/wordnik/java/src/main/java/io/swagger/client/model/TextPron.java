package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class TextPron {

    private String raw = null;
    private Integer seq = null;
    private String rawType = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("raw")
    public String getRaw() {
        return raw;
    }

    public void setRaw(String raw) {
        this.raw = raw;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("seq")
    public Integer getSeq() {
        return seq;
    }

    public void setSeq(Integer seq) {
        this.seq = seq;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("rawType")
    public String getRawType() {
        return rawType;
    }

    public void setRawType(String rawType) {
        this.rawType = rawType;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class TextPron {\n");

        sb.append("  raw: ").append(raw).append("\n");
        sb.append("  seq: ").append(seq).append("\n");
        sb.append("  rawType: ").append(rawType).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
