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
}

