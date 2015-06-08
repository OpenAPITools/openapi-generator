package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.WordSearchResult;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class WordSearchResults {

    private List<WordSearchResult> searchResults = new ArrayList<WordSearchResult>();
    private Integer totalResults = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("searchResults")
    public List<WordSearchResult> getSearchResults() {
        return searchResults;
    }

    public void setSearchResults(List<WordSearchResult> searchResults) {
        this.searchResults = searchResults;
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
        sb.append("class WordSearchResults {\n");

        sb.append("  searchResults: ").append(searchResults).append("\n");
        sb.append("  totalResults: ").append(totalResults).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
