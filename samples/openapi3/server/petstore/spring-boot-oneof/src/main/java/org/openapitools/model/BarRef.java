package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
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
public class BarRef extends EntityRef implements BarRefOrValue {

  public BarRef() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public BarRef(String atType) {
    super(atType);
  }


  public BarRef name(String name) {
    super.name(name);
    return this;
  }

  public BarRef atReferredType(String atReferredType) {
    super.atReferredType(atReferredType);
    return this;
  }

  public BarRef href(String href) {
    super.href(href);
    return this;
  }

  public BarRef id(String id) {
    super.id(id);
    return this;
  }

  public BarRef atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public BarRef atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public BarRef atType(String atType) {
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
    return true;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BarRef {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

