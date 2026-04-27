package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Shared pagination metadata schema
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class PageMeta implements Serializable {

  private static final long serialVersionUID = 1L;

  private Long size;

  private Long number;

  private Long totalElements;

  private Long totalPages;

  public PageMeta() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public PageMeta(Long size, Long number, Long totalElements, Long totalPages) {
    this.size = size;
    this.number = number;
    this.totalElements = totalElements;
    this.totalPages = totalPages;
  }

  public PageMeta size(Long size) {
    this.size = size;
    return this;
  }

  /**
   * Get size
   * @return size
   */
  @NotNull 
  @JsonProperty("size")
  public Long getSize() {
    return size;
  }

  @JsonProperty("size")
  public void setSize(Long size) {
    this.size = size;
  }

  public PageMeta number(Long number) {
    this.number = number;
    return this;
  }

  /**
   * Get number
   * @return number
   */
  @NotNull 
  @JsonProperty("number")
  public Long getNumber() {
    return number;
  }

  @JsonProperty("number")
  public void setNumber(Long number) {
    this.number = number;
  }

  public PageMeta totalElements(Long totalElements) {
    this.totalElements = totalElements;
    return this;
  }

  /**
   * Get totalElements
   * @return totalElements
   */
  @NotNull 
  @JsonProperty("totalElements")
  public Long getTotalElements() {
    return totalElements;
  }

  @JsonProperty("totalElements")
  public void setTotalElements(Long totalElements) {
    this.totalElements = totalElements;
  }

  public PageMeta totalPages(Long totalPages) {
    this.totalPages = totalPages;
    return this;
  }

  /**
   * Get totalPages
   * @return totalPages
   */
  @NotNull 
  @JsonProperty("totalPages")
  public Long getTotalPages() {
    return totalPages;
  }

  @JsonProperty("totalPages")
  public void setTotalPages(Long totalPages) {
    this.totalPages = totalPages;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PageMeta pageMeta = (PageMeta) o;
    return Objects.equals(this.size, pageMeta.size) &&
        Objects.equals(this.number, pageMeta.number) &&
        Objects.equals(this.totalElements, pageMeta.totalElements) &&
        Objects.equals(this.totalPages, pageMeta.totalPages);
  }

  @Override
  public int hashCode() {
    return Objects.hash(size, number, totalElements, totalPages);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PageMeta {\n");
    sb.append("    size: ").append(toIndentedString(size)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    totalElements: ").append(toIndentedString(totalElements)).append("\n");
    sb.append("    totalPages: ").append(toIndentedString(totalPages)).append("\n");
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

