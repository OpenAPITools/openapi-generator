package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Square
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Square implements Shape {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Double side;

  public Square() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Square(Double side) {
    this.side = side;
  }

  public Square side(Double side) {
    this.side = side;
    return this;
  }

  /**
   * Get side
   * @return side
   */
  @NotNull 
  @Schema(name = "side", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("side")
  public Double getSide() {
    return side;
  }

  @JsonProperty("side")
  public void setSide(Double side) {
    this.side = side;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Square square = (Square) o;
    return Objects.equals(this.side, square.side);
  }

  @Override
  public int hashCode() {
    return Objects.hash(side);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Square {\n");
    sb.append("    side: ").append(toIndentedString(side)).append("\n");
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
  
  public static class Builder {

    private Square instance;

    public Builder() {
      this(new Square());
    }

    protected Builder(Square instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Square value) { 
      this.instance.setSide(value.side);
      return this;
    }

    public Square.Builder side(Double side) {
      this.instance.side(side);
      return this;
    }
    
    /**
    * returns a built Square instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Square build() {
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
  public static Square.Builder builder() {
    return new Square.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Square.Builder toBuilder() {
    Square.Builder builder = new Square.Builder();
    return builder.copyOf(this);
  }

}

