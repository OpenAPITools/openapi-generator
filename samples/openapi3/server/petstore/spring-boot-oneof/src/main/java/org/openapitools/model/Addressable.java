package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Base schema for addressable entities
 */

@Schema(name = "Addressable", description = "Base schema for addressable entities")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class Addressable {

  private @Nullable String href;

  private @Nullable String id;

  public Addressable href(@Nullable String href) {
    this.href = href;
    return this;
  }

  /**
   * Hyperlink reference
   * @return href
   */
  
  @Schema(name = "href", description = "Hyperlink reference", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("href")
  public @Nullable String getHref() {
    return href;
  }

  public void setHref(@Nullable String href) {
    this.href = href;
  }

  public Addressable id(@Nullable String id) {
    this.id = id;
    return this;
  }

  /**
   * unique identifier
   * @return id
   */
  
  @Schema(name = "id", description = "unique identifier", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  public @Nullable String getId() {
    return id;
  }

  public void setId(@Nullable String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Addressable addressable = (Addressable) o;
    return Objects.equals(this.href, addressable.href) &&
        Objects.equals(this.id, addressable.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(href, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Addressable {\n");
    sb.append("    href: ").append(toIndentedString(href)).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
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

    private Addressable instance;

    public Builder() {
      this(new Addressable());
    }

    protected Builder(Addressable instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Addressable value) { 
      this.instance.setHref(value.href);
      this.instance.setId(value.id);
      return this;
    }

    public Addressable.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    public Addressable.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    /**
    * returns a built Addressable instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Addressable build() {
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
  public static Addressable.Builder builder() {
    return new Addressable.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Addressable.Builder toBuilder() {
    Addressable.Builder builder = new Addressable.Builder();
    return builder.copyOf(this);
  }

}

