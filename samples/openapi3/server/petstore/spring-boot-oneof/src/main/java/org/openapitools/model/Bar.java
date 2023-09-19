package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
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
 * Bar
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Bar extends Entity implements BarRefOrValue {

  private String id;

  private String barPropA;

  private String fooPropB;

  private FooRefOrValue foo;

  public Bar() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Bar(String id, String atType) {
    super(atType);
    this.id = id;
  }

  public Bar id(String id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @NotNull 
  @Schema(name = "id", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("id")
  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Bar barPropA(String barPropA) {
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

  public Bar fooPropB(String fooPropB) {
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

  public Bar foo(FooRefOrValue foo) {
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


  public Bar href(String href) {
    super.href(href);
    return this;
  }

  public Bar atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public Bar atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public Bar atType(String atType) {
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
    Bar bar = (Bar) o;
    return Objects.equals(this.id, bar.id) &&
        Objects.equals(this.barPropA, bar.barPropA) &&
        Objects.equals(this.fooPropB, bar.fooPropB) &&
        Objects.equals(this.foo, bar.foo) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, barPropA, fooPropB, foo, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Bar {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
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

