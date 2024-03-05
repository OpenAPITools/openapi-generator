package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.Entity;
import org.openapitools.model.FooRefOrValue;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * BarCreate
 */


@JsonTypeName("Bar_Create")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class BarCreate extends Entity {

  private String barPropA;

  private String fooPropB;

  private FooRefOrValue foo;

  public BarCreate() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public BarCreate(String atType) {
    super(atType);
  }

  public BarCreate barPropA(String barPropA) {
    this.barPropA = barPropA;
    return this;
  }

  /**
   * Get barPropA
   * @return barPropA
  */
  
  @Schema(name = "barPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("barPropA")
  public String getBarPropA() {
    return barPropA;
  }

  public void setBarPropA(String barPropA) {
    this.barPropA = barPropA;
  }

  public BarCreate fooPropB(String fooPropB) {
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

  public BarCreate foo(FooRefOrValue foo) {
    this.foo = foo;
    return this;
  }

  /**
   * Get foo
   * @return foo
  */
  @Valid 
  @Schema(name = "foo", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("foo")
  public FooRefOrValue getFoo() {
    return foo;
  }

  public void setFoo(FooRefOrValue foo) {
    this.foo = foo;
  }


  public BarCreate href(String href) {
    super.href(href);
    return this;
  }

  public BarCreate id(String id) {
    super.id(id);
    return this;
  }

  public BarCreate atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public BarCreate atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public BarCreate atType(String atType) {
    super.atType(atType);
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
    BarCreate barCreate = (BarCreate) o;
    return Objects.equals(this.barPropA, barCreate.barPropA) &&
        Objects.equals(this.fooPropB, barCreate.fooPropB) &&
        Objects.equals(this.foo, barCreate.foo) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(barPropA, fooPropB, foo, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BarCreate {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    barPropA: ").append(toIndentedString(barPropA)).append("\n");
    sb.append("    fooPropB: ").append(toIndentedString(fooPropB)).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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

