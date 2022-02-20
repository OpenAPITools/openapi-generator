package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Optional;
import org.openapitools.model.Animal;
import org.openapitools.model.CatAllOf;
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

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Cat extends Animal  {

  @JsonProperty("declawed")
  private Optional<Boolean> declawed = Optional.empty();

  public Cat declawed(Boolean declawed) {
    this.declawed = Optional.ofNullable(declawed);
    return this;
  }

  /**
   * Get declawed
   * @return declawed
  */
  @Schema(name = "declawed", required = false)
  public Optional<Boolean> getDeclawed() {
    return declawed;
  }

  @JsonIgnore
  public void setDeclawed(Boolean declawed) {
    this.declawed = Optional.ofNullable(declawed);
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
    return Objects.equals(this.declawed, cat.declawed) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(declawed, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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
}

