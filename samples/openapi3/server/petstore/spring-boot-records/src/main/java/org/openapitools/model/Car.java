package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Vehicle;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Car
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Car extends Vehicle {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer doors;

  public Car() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Car(String type) {
    super(type);
  }

  public Car doors(@Nullable Integer doors) {
    this.doors = doors;
    return this;
  }

  /**
   * Get doors
   * @return doors
   */
  
  @Schema(name = "doors", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("doors")
  public @Nullable Integer getDoors() {
    return doors;
  }

  @JsonProperty("doors")
  public void setDoors(@Nullable Integer doors) {
    this.doors = doors;
  }


  public Car type(String type) {
    super.type(type);
    return this;
  }

  public Car wheels(Integer wheels) {
    super.wheels(wheels);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Car car = (Car) o;
    return Objects.equals(this.doors, car.doors) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(doors, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Car {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    doors: ").append(toIndentedString(doors)).append("\n");
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

