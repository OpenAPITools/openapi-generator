package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.Definition;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class DefinitionSearchResults {

    private List<Definition> results = new ArrayList<Definition>();
    private Integer totalResults = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("results")
    public List<Definition> getResults() {
        return results;
    }

    public void setResults(List<Definition> results) {
        this.results = results;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("totalResults")
    public Integer getTotalResults() {
        return totalResults;
    }

    public void setTotalResults(Integer totalResults) {
        this.totalResults = totalResults;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class DefinitionSearchResults {\n");

        sb.append("  results: ").append(results).append("\n");
        sb.append("  totalResults: ").append(totalResults).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
