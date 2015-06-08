package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Citation {

    private String cite = null;
    private String source = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("cite")
    public String getCite() {
        return cite;
    }

    public void setCite(String cite) {
        this.cite = cite;
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


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Citation {\n");

        sb.append("  cite: ").append(cite).append("\n");
        sb.append("  source: ").append(source).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
