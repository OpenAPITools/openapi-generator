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
 * Cat
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public final class Cat implements Animal {

  private @Nullable Boolean declawed;

  public Cat declawed(@Nullable Boolean declawed) {
    this.declawed = declawed;
    return this;
  }

  /**
   * Get declawed
   * @return declawed
   */
  
  @Schema(name = "declawed", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("declawed")
  public @Nullable Boolean getDeclawed() {
    return declawed;
  }

  public void setDeclawed(@Nullable Boolean declawed) {
    this.declawed = declawed;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Cat cat = (Cat) o;
    return Objects.equals(this.declawed, cat.declawed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(declawed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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

    private Cat instance;

    public Builder() {
      this(new Cat());
    }

    protected Builder(Cat instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Cat value) { 
      this.instance.setDeclawed(value.declawed);
      return this;
    }

    public Cat.Builder declawed(Boolean declawed) {
      this.instance.declawed(declawed);
      return this;
    }
    
    /**
    * returns a built Cat instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Cat build() {
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
  public static Cat.Builder builder() {
    return new Cat.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Cat.Builder toBuilder() {
    Cat.Builder builder = new Cat.Builder();
    return builder.copyOf(this);
  }

}

