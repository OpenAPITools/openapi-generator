package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.EntityRef;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * FooRef
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class FooRef extends EntityRef implements FooRefOrValue, FooRefOrValueEnumMapping {

  @JsonProperty("foorefPropA")
  private String foorefPropA;

  @JsonProperty("objectType")
  private RefOrValueEnum objectType;

  public FooRef foorefPropA(String foorefPropA) {
    this.foorefPropA = foorefPropA;
    return this;
  }

  /**
   * Get foorefPropA
   * @return foorefPropA
  */
  
  @Schema(name = "foorefPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public String getFoorefPropA() {
    return foorefPropA;
  }

  public void setFoorefPropA(String foorefPropA) {
    this.foorefPropA = foorefPropA;
  }

  public FooRef objectType(RefOrValueEnum objectType) {
    this.objectType = objectType;
    return this;
  }

  /**
   * Get objectType
   * @return objectType
  */
  @NotNull @Valid 
  @Schema(name = "objectType", requiredMode = Schema.RequiredMode.REQUIRED)
  public RefOrValueEnum getObjectType() {
    return objectType;
  }

  public void setObjectType(RefOrValueEnum objectType) {
    this.objectType = objectType;
  }

  public FooRef name(String name) {
    super.setName(name);
    return this;
  }

  public FooRef atReferredType(String atReferredType) {
    super.setAtReferredType(atReferredType);
    return this;
  }

  public FooRef href(String href) {
    super.setHref(href);
    return this;
  }

  public FooRef id(String id) {
    super.setId(id);
    return this;
  }

  public FooRef atSchemaLocation(String atSchemaLocation) {
    super.setAtSchemaLocation(atSchemaLocation);
    return this;
  }

  public FooRef atBaseType(String atBaseType) {
    super.setAtBaseType(atBaseType);
    return this;
  }

  public FooRef atType(String atType) {
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
    FooRef fooRef = (FooRef) o;
    return Objects.equals(this.foorefPropA, fooRef.foorefPropA) &&
        Objects.equals(this.objectType, fooRef.objectType) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(foorefPropA, objectType, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FooRef {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    foorefPropA: ").append(toIndentedString(foorefPropA)).append("\n");
    sb.append("    objectType: ").append(toIndentedString(objectType)).append("\n");
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

