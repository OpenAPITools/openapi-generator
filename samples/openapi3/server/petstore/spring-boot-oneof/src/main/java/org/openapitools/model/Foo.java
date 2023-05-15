package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Entity;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Foo
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Foo extends Entity implements FooRefOrValue {

  private String fooPropA;

  private String fooPropB;

  public Foo() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Foo(String atType) {
    super(atType);
  }

  public Foo fooPropA(String fooPropA) {
    this.fooPropA = fooPropA;
    return this;
  }

  /**
   * Get fooPropA
   * @return fooPropA
  */
  
  @Schema(name = "fooPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("fooPropA")
  public String getFooPropA() {
    return fooPropA;
  }

  public void setFooPropA(String fooPropA) {
    this.fooPropA = fooPropA;
  }

  public Foo fooPropB(String fooPropB) {
    this.fooPropB = fooPropB;
    return this;
  }

  /**
   * Get fooPropB
   * @return fooPropB
  */
  
  @Schema(name = "fooPropB", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("fooPropB")
  public String getFooPropB() {
    return fooPropB;
  }

  public void setFooPropB(String fooPropB) {
    this.fooPropB = fooPropB;
  }

  public Foo href(String href) {
    super.setHref(href);
    return this;
  }

  public Foo id(String id) {
    super.setId(id);
    return this;
  }

  public Foo atSchemaLocation(String atSchemaLocation) {
    super.setAtSchemaLocation(atSchemaLocation);
    return this;
  }

  public Foo atBaseType(String atBaseType) {
    super.setAtBaseType(atBaseType);
    return this;
  }

  public Foo atType(String atType) {
    super.setAtType(atType);
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
    Foo foo = (Foo) o;
    return Objects.equals(this.fooPropA, foo.fooPropA) &&
        Objects.equals(this.fooPropB, foo.fooPropB) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(fooPropA, fooPropB, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Foo {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    fooPropA: ").append(toIndentedString(fooPropA)).append("\n");
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

