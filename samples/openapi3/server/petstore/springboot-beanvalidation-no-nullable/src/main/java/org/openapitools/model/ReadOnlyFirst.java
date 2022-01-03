package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;

/**
 * ReadOnlyFirst
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ReadOnlyFirst   {
  @JsonProperty("bar")
  private String bar;

  @JsonProperty("baz")
  private String baz;

  public ReadOnlyFirst bar(String bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
  */
  @Schema(name = "bar", readOnly = true, defaultValue = "")


  public String getBar() {
    return bar;
  }

  public void setBar(String bar) {
    this.bar = bar;
  }

  public ReadOnlyFirst baz(String baz) {
    this.baz = baz;
    return this;
  }

  /**
   * Get baz
   * @return baz
  */
  @Schema(name = "baz", defaultValue = "")


  public String getBaz() {
    return baz;
  }

  public void setBaz(String baz) {
    this.baz = baz;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ReadOnlyFirst readOnlyFirst = (ReadOnlyFirst) o;
    return Objects.equals(this.bar, readOnlyFirst.bar) &&
        Objects.equals(this.baz, readOnlyFirst.baz);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, baz);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReadOnlyFirst {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    baz: ").append(toIndentedString(baz)).append("\n");
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

