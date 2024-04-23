package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * CategoryDto
 */

@JsonTypeName("Category")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class CategoryDto {

  private Long id;

  private String name = "default-name";

  public CategoryDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public CategoryDto(String name) {
    this.name = name;
  }

  public CategoryDto id(Long id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public CategoryDto name(String name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CategoryDto category = (CategoryDto) o;
    return Objects.equals(this.id, category.id) &&
        Objects.equals(this.name, category.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CategoryDto {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private CategoryDto instance;

    public Builder() {
      this(new CategoryDto());
    }

    protected Builder(CategoryDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(CategoryDto value) { 
      this.instance.setId(value.id);
      this.instance.setName(value.name);
      return this;
    }

    public CategoryDto.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    
    public CategoryDto.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    /**
    * returns a built CategoryDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public CategoryDto build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static CategoryDto.Builder builder() {
    return new CategoryDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public CategoryDto.Builder toBuilder() {
    CategoryDto.Builder builder = new CategoryDto.Builder();
    return builder.copyOf(this);
  }

}

