package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class FacetValue {

    private Long count = null;
    private String value = null;


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
    @JsonProperty("value")
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FacetValue {\n");

        sb.append("  count: ").append(count).append("\n");
        sb.append("  value: ").append(value).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
