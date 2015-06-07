package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.Frequency;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class FrequencySummary {

    private Integer unknownYearCount = null;
    private Long totalCount = null;
    private String frequencyString = null;
    private String word = null;
    private List<Frequency> frequency = new ArrayList<Frequency>();


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("unknownYearCount")
    public Integer getUnknownYearCount() {
        return unknownYearCount;
    }

    public void setUnknownYearCount(Integer unknownYearCount) {
        this.unknownYearCount = unknownYearCount;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("totalCount")
    public Long getTotalCount() {
        return totalCount;
    }

    public void setTotalCount(Long totalCount) {
        this.totalCount = totalCount;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("frequencyString")
    public String getFrequencyString() {
        return frequencyString;
    }

    public void setFrequencyString(String frequencyString) {
        this.frequencyString = frequencyString;
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


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("frequency")
    public List<Frequency> getFrequency() {
        return frequency;
    }

    public void setFrequency(List<Frequency> frequency) {
        this.frequency = frequency;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FrequencySummary {\n");

        sb.append("  unknownYearCount: ").append(unknownYearCount).append("\n");
        sb.append("  totalCount: ").append(totalCount).append("\n");
        sb.append("  frequencyString: ").append(frequencyString).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("  frequency: ").append(frequency).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
