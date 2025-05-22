package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ReadOnlyFirst
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class ReadOnlyFirst {

  private Optional<String> bar = Optional.empty();

  private Optional<String> baz = Optional.empty();

  public ReadOnlyFirst bar(String bar) {
    this.bar = Optional.ofNullable(bar);
    return this;
  }

  /**
   * Get bar
   * @return bar
   */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("bar")
  public Optional<String> getBar() {
    return bar;
  }

  public void setBar(Optional<String> bar) {
    this.bar = bar;
  }

  public ReadOnlyFirst baz(String baz) {
    this.baz = Optional.ofNullable(baz);
    return this;
  }

  /**
   * Get baz
   * @return baz
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("baz")
  public Optional<String> getBaz() {
    return baz;
  }

  public void setBaz(Optional<String> baz) {
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

    protected Builder copyOf(ReadOnlyFirst value) { 
      this.instance.setBar(value.bar);
      this.instance.setBaz(value.baz);
      return this;
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
  * Create a builder with no initialized field (except for the default values).
  */
  public static ReadOnlyFirst.Builder builder() {
    return new ReadOnlyFirst.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ReadOnlyFirst.Builder toBuilder() {
    ReadOnlyFirst.Builder builder = new ReadOnlyFirst.Builder();
    return builder.copyOf(this);
  }

}

