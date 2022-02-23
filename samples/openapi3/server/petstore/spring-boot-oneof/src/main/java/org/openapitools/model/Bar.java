package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.Entity;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Bar
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Bar extends Entity {

  @JsonProperty("barPropA")
  private String barPropA;

  @JsonProperty("fooPropB")
  private String fooPropB;

  public Bar barPropA(String barPropA) {
    this.barPropA = barPropA;
    return this;
  }

  /**
   * Get barPropA
   * @return barPropA
  */
  
  @Schema(name = "barPropA", required = false)
  public String getBarPropA() {
    return barPropA;
  }

  public void setBarPropA(String barPropA) {
    this.barPropA = barPropA;
  }

  public Bar fooPropB(String fooPropB) {
    this.fooPropB = fooPropB;
    return this;
  }

  /**
   * Get fooPropB
   * @return fooPropB
  */
  
  @Schema(name = "fooPropB", required = false)
  public String getFooPropB() {
    return fooPropB;
  }

  public void setFooPropB(String fooPropB) {
    this.fooPropB = fooPropB;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Bar bar = (Bar) o;
    return Objects.equals(this.barPropA, bar.barPropA) &&
        Objects.equals(this.fooPropB, bar.fooPropB) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(barPropA, fooPropB, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Bar {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    barPropA: ").append(toIndentedString(barPropA)).append("\n");
    sb.append("    fooPropB: ").append(toIndentedString(fooPropB)).append("\n");
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

