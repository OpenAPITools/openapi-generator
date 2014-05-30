package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.*;
import com.wordnik.client.model.WordSearchResult;
public class WordSearchResults {
  @JsonProperty("searchResults")
  private List<WordSearchResult> searchResults = new ArrayList<WordSearchResult>();
  @JsonProperty("totalResults")
  private Integer totalResults = null;
  public List<WordSearchResult> getSearchResults() {
    return searchResults;
  }
  public void setSearchResults(List<WordSearchResult> searchResults) {
    this.searchResults = searchResults;
  }

  public Integer getTotalResults() {
    return totalResults;
  }
  public void setTotalResults(Integer totalResults) {
    this.totalResults = totalResults;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordSearchResults {\n");
    sb.append("  searchResults: ").append(searchResults).append("\n");
    sb.append("  totalResults: ").append(totalResults).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

