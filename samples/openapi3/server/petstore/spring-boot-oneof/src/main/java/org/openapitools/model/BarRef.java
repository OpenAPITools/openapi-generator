package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.EntityRef;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * BarRef
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class BarRef extends EntityRef {

  @JsonProperty("barRefPropA")
  private String barRefPropA;

  public BarRef barRefPropA(String barRefPropA) {
    this.barRefPropA = barRefPropA;
    return this;
  }

  /**
   * Get barRefPropA
   * @return barRefPropA
  */
  
  @Schema(name = "barRefPropA", required = false)
  public String getBarRefPropA() {
    return barRefPropA;
  }

  public void setBarRefPropA(String barRefPropA) {
    this.barRefPropA = barRefPropA;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    BarRef barRef = (BarRef) o;
    return Objects.equals(this.barRefPropA, barRef.barRefPropA) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(barRefPropA, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BarRef {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    barRefPropA: ").append(toIndentedString(barRefPropA)).append("\n");
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

