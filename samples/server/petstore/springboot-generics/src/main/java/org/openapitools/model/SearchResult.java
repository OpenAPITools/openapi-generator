package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Search result — unique structure, not matched by any pattern
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class SearchResult implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable String query;

  private @Nullable Long totalHits;

  private List<String> results = new ArrayList<>();

  private Map<String, Integer> facets = new HashMap<>();

  public SearchResult query(@Nullable String query) {
    this.query = query;
    return this;
  }

  /**
   * Get query
   * @return query
   */
  
  @JsonProperty("query")
  public @Nullable String getQuery() {
    return query;
  }

  @JsonProperty("query")
  public void setQuery(@Nullable String query) {
    this.query = query;
  }

  public SearchResult totalHits(@Nullable Long totalHits) {
    this.totalHits = totalHits;
    return this;
  }

  /**
   * Get totalHits
   * @return totalHits
   */
  
  @JsonProperty("totalHits")
  public @Nullable Long getTotalHits() {
    return totalHits;
  }

  @JsonProperty("totalHits")
  public void setTotalHits(@Nullable Long totalHits) {
    this.totalHits = totalHits;
  }

  public SearchResult results(List<String> results) {
    this.results = results;
    return this;
  }

  public SearchResult addResultsItem(String resultsItem) {
    if (this.results == null) {
      this.results = new ArrayList<>();
    }
    this.results.add(resultsItem);
    return this;
  }

  /**
   * Get results
   * @return results
   */
  
  @JsonProperty("results")
  public List<String> getResults() {
    return results;
  }

  @JsonProperty("results")
  public void setResults(List<String> results) {
    this.results = results;
  }

  public SearchResult facets(Map<String, Integer> facets) {
    this.facets = facets;
    return this;
  }

  public SearchResult putFacetsItem(String key, Integer facetsItem) {
    if (this.facets == null) {
      this.facets = new HashMap<>();
    }
    this.facets.put(key, facetsItem);
    return this;
  }

  /**
   * Get facets
   * @return facets
   */
  
  @JsonProperty("facets")
  public Map<String, Integer> getFacets() {
    return facets;
  }

  @JsonProperty("facets")
  public void setFacets(Map<String, Integer> facets) {
    this.facets = facets;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SearchResult searchResult = (SearchResult) o;
    return Objects.equals(this.query, searchResult.query) &&
        Objects.equals(this.totalHits, searchResult.totalHits) &&
        Objects.equals(this.results, searchResult.results) &&
        Objects.equals(this.facets, searchResult.facets);
  }

  @Override
  public int hashCode() {
    return Objects.hash(query, totalHits, results, facets);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SearchResult {\n");
    sb.append("    query: ").append(toIndentedString(query)).append("\n");
    sb.append("    totalHits: ").append(toIndentedString(totalHits)).append("\n");
    sb.append("    results: ").append(toIndentedString(results)).append("\n");
    sb.append("    facets: ").append(toIndentedString(facets)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

