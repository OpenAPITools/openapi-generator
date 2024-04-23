package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ReadOnlyFirstDto
 */

@JsonTypeName("ReadOnlyFirst")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class ReadOnlyFirstDto {

  private String bar;

  private String baz;

  public ReadOnlyFirstDto bar(String bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
  */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  public void setBar(String bar) {
    this.bar = bar;
  }

  public ReadOnlyFirstDto baz(String baz) {
    this.baz = baz;
    return this;
  }

  /**
   * Get baz
   * @return baz
  */
  
  @ApiModelProperty(value = "")
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
    ReadOnlyFirstDto readOnlyFirst = (ReadOnlyFirstDto) o;
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
    sb.append("class ReadOnlyFirstDto {\n");
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

    private ReadOnlyFirstDto instance;

    public Builder() {
      this(new ReadOnlyFirstDto());
    }

    protected Builder(ReadOnlyFirstDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ReadOnlyFirstDto value) { 
      this.instance.setBar(value.bar);
      this.instance.setBaz(value.baz);
      return this;
    }

    public ReadOnlyFirstDto.Builder bar(String bar) {
      this.instance.bar(bar);
      return this;
    }
    
    public ReadOnlyFirstDto.Builder baz(String baz) {
      this.instance.baz(baz);
      return this;
    }
    
    /**
    * returns a built ReadOnlyFirstDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ReadOnlyFirstDto build() {
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
  public static ReadOnlyFirstDto.Builder builder() {
    return new ReadOnlyFirstDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ReadOnlyFirstDto.Builder toBuilder() {
    ReadOnlyFirstDto.Builder builder = new ReadOnlyFirstDto.Builder();
    return builder.copyOf(this);
  }

}

