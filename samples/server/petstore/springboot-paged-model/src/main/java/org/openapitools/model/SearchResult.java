package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.model.PageMeta;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Search result with metadata — no &#39;content&#39; array at all
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class SearchResult implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable String query;

  private @Nullable Integer totalHits;

  private @Nullable PageMeta page;

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

  public SearchResult totalHits(@Nullable Integer totalHits) {
    this.totalHits = totalHits;
    return this;
  }

  /**
   * Get totalHits
   * @return totalHits
   */
  
  @JsonProperty("totalHits")
  public @Nullable Integer getTotalHits() {
    return totalHits;
  }

  @JsonProperty("totalHits")
  public void setTotalHits(@Nullable Integer totalHits) {
    this.totalHits = totalHits;
  }

  public SearchResult page(@Nullable PageMeta page) {
    this.page = page;
    return this;
  }

  /**
   * Get page
   * @return page
   */
  @Valid 
  @JsonProperty("page")
  public @Nullable PageMeta getPage() {
    return page;
  }

  @JsonProperty("page")
  public void setPage(@Nullable PageMeta page) {
    this.page = page;
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
        Objects.equals(this.page, searchResult.page);
  }

  @Override
  public int hashCode() {
    return Objects.hash(query, totalHits, page);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SearchResult {\n");
    sb.append("    query: ").append(toIndentedString(query)).append("\n");
    sb.append("    totalHits: ").append(toIndentedString(totalHits)).append("\n");
    sb.append("    page: ").append(toIndentedString(page)).append("\n");
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

