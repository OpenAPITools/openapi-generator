package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ReadOnlyFirst
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ReadOnlyFirst {

  private String bar;

  private String baz;

  public ReadOnlyFirst bar(String bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
  */
  
  @JsonProperty("bar")
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
  
  @JsonProperty("baz")
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
  
  public static class Builder {

    private ReadOnlyFirst instance;

    public Builder() {
      this(new ReadOnlyFirst());
    }

    protected Builder(ReadOnlyFirst instance) {
      this.instance = instance;
    }

    public ReadOnlyFirst.Builder bar(String bar) {
      this.instance.bar(bar);
      return this;
    }
    public ReadOnlyFirst.Builder baz(String baz) {
      this.instance.baz(baz);
      return this;
    }
    /**
    * returns a built ReadOnlyFirst instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ReadOnlyFirst build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
  }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field.
  */
  public static ReadOnlyFirst.Builder builder() {
    return new ReadOnlyFirst.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ReadOnlyFirst.Builder toBuilder() {
    ReadOnlyFirst.Builder builder = new ReadOnlyFirst.Builder();
    builder.instance.setBar(bar);
    builder.instance.setBaz(baz);
    return builder;
  }

}

