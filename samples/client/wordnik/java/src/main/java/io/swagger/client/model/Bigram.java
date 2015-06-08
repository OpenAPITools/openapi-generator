package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Bigram {

    private Long count = null;
    private String gram2 = null;
    private String gram1 = null;
    private Double wlmi = null;
    private Double mi = null;


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
    @JsonProperty("gram2")
    public String getGram2() {
        return gram2;
    }

    public void setGram2(String gram2) {
        this.gram2 = gram2;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("gram1")
    public String getGram1() {
        return gram1;
    }

    public void setGram1(String gram1) {
        this.gram1 = gram1;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("wlmi")
    public Double getWlmi() {
        return wlmi;
    }

    public void setWlmi(Double wlmi) {
        this.wlmi = wlmi;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("mi")
    public Double getMi() {
        return mi;
    }

    public void setMi(Double mi) {
        this.mi = mi;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Bigram {\n");

        sb.append("  count: ").append(count).append("\n");
        sb.append("  gram2: ").append(gram2).append("\n");
        sb.append("  gram1: ").append(gram1).append("\n");
        sb.append("  wlmi: ").append(wlmi).append("\n");
        sb.append("  mi: ").append(mi).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
