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
 * Pasta
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Pasta extends Entity {

  private String vendor;

  public Pasta() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Pasta(String atType) {
    super(atType);
  }

  public Pasta vendor(String vendor) {
    this.vendor = vendor;
    return this;
  }

  /**
   * Get vendor
   * @return vendor
  */
  
  @Schema(name = "vendor", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("vendor")
  public String getVendor() {
    return vendor;
  }

  public void setVendor(String vendor) {
    this.vendor = vendor;
  }

  public Pasta href(String href) {
    super.setHref(href);
    return this;
  }

  public Pasta id(String id) {
    super.setId(id);
    return this;
  }

  public Pasta atSchemaLocation(String atSchemaLocation) {
    super.setAtSchemaLocation(atSchemaLocation);
    return this;
  }

  public Pasta atBaseType(String atBaseType) {
    super.setAtBaseType(atBaseType);
    return this;
  }

  public Pasta atType(String atType) {
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
    Pasta pasta = (Pasta) o;
    return Objects.equals(this.vendor, pasta.vendor) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(vendor, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pasta {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    vendor: ").append(toIndentedString(vendor)).append("\n");
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

