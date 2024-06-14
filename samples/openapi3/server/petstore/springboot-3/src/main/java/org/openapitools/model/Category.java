package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * A category for a pet
 */

@Schema(name = "Category", description = "A category for a pet")
@JacksonXmlRootElement(localName = "Category")
@XmlRootElement(name = "Category")
@XmlAccessorType(XmlAccessType.FIELD)

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class Category {

  private Long id;

  private String name;

  public Category() {
    super();
  }

  /**
  * Constructor with all args parameters
  */
  public Category(Long id, String name) {
      this.id = id;
      this.name = name;
  }

  public Category id(Long id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
  */
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JacksonXmlProperty(localName = "id")
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Category name(String name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
  */
  @Pattern(regexp = "^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$") 
  @Schema(name = "name", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name")
  @JacksonXmlProperty(localName = "name")
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
    Category category = (Category) o;
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
    sb.append("class Category {\n");
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

    private Category instance;

    public Builder() {
      this(new Category());
    }

    protected Builder(Category instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Category value) { 
      this.instance.setId(value.id);
      this.instance.setName(value.name);
      return this;
    }

    public Category.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    
    public Category.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    /**
    * returns a built Category instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Category build() {
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
  public static Category.Builder builder() {
    return new Category.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Category.Builder toBuilder() {
    Category.Builder builder = new Category.Builder();
    return builder.copyOf(this);
  }

}

