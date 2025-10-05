package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ReadOnlyFirstDto
 */

@JsonTypeName("ReadOnlyFirst")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ReadOnlyFirstDto {

  private @Nullable String bar;

  private @Nullable String baz;

  public ReadOnlyFirstDto bar(@Nullable String bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
   */
  
  @JsonProperty("bar")
  public @Nullable String getBar() {
    return bar;
  }

  public void setBar(@Nullable String bar) {
    this.bar = bar;
  }

  public ReadOnlyFirstDto baz(@Nullable String baz) {
    this.baz = baz;
    return this;
  }

  /**
   * Get baz
   * @return baz
   */
  
  @JsonProperty("baz")
  public @Nullable String getBaz() {
    return baz;
  }

  public void setBaz(@Nullable String baz) {
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
}

