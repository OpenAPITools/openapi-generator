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
 * Circle
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Circle implements Shape {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Double radius;

  public Circle() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Circle(Double radius) {
    this.radius = radius;
  }

  public Circle radius(Double radius) {
    this.radius = radius;
    return this;
  }

  /**
   * Get radius
   * @return radius
   */
  @NotNull 
  @Schema(name = "radius", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("radius")
  public Double getRadius() {
    return radius;
  }

  @JsonProperty("radius")
  public void setRadius(Double radius) {
    this.radius = radius;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Circle circle = (Circle) o;
    return Objects.equals(this.radius, circle.radius);
  }

  @Override
  public int hashCode() {
    return Objects.hash(radius);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Circle {\n");
    sb.append("    radius: ").append(toIndentedString(radius)).append("\n");
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

