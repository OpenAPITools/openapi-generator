package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Range;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Query parameters of the pet search
 *
 * @param name Get name
 * @param limit Get limit
 * @param tags Get tags
 * @param sortOrder Get sortOrder
 * @param range Get range
 */

@Schema(name = "Filter", description = "Query parameters of the pet search")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Filter(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "name", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("name")
    @Nullable String name,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "limit", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("limit")
    Integer limit,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "tags", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("tags")
    List<String> tags,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "sort-order", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @org.springframework.web.bind.annotation.BindParam("sort-order")
    @JsonProperty("sort-order")
    @Nullable String sortOrder,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Valid 
    @Schema(name = "range", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("range")
    @Nullable Range range
) {

  public Filter {
    if (limit == null) {
      limit = 10;
    }
    if (tags == null) {
      tags = new ArrayList<>();
    }
  }

  /**
   * Builder of Filter. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String name;

    private Integer limit;

    private List<String> tags;

    private String sortOrder;

    private Range range;

    public Builder() {
    }

    protected Builder copyOf(Filter value) {
      this.name = value.name();
      this.limit = value.limit();
      this.tags = value.tags();
      this.sortOrder = value.sortOrder();
      this.range = value.range();
      return this;
    }

    public Filter.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Filter.Builder limit(Integer limit) {
      this.limit = limit;
      return this;
    }

    public Filter.Builder tags(List<String> tags) {
      this.tags = tags;
      return this;
    }

    public Filter.Builder sortOrder(String sortOrder) {
      this.sortOrder = sortOrder;
      return this;
    }

    public Filter.Builder range(Range range) {
      this.range = range;
      return this;
    }

    /**
     * Returns a new Filter from the values set so far; the builder stays usable.
     */
    public Filter build() {
      return new Filter(this.name, this.limit, this.tags, this.sortOrder, this.range);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Filter.Builder builder() {
    return new Filter.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Filter.Builder toBuilder() {
    return new Filter.Builder().copyOf(this);
  }

}

