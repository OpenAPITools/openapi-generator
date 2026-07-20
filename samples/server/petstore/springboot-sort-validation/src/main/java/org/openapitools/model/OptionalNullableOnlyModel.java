package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
import java.util.UUID;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * OptionalNullableOnlyModel
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class OptionalNullableOnlyModel implements Serializable {

  private static final long serialVersionUID = 1L;

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  private JsonNullable<UUID> optionalNullable = JsonNullable.<UUID>undefined();

  public OptionalNullableOnlyModel optionalNullable(UUID optionalNullable) {
    this.optionalNullable = JsonNullable.of(optionalNullable);
    return this;
  }

  /**
   * Get optionalNullable
   * @return optionalNullable
   */
  @Valid 
  @JsonProperty("optionalNullable")
  public JsonNullable<UUID> getOptionalNullable() {
    return optionalNullable;
  }

  public void setOptionalNullable(JsonNullable<UUID> optionalNullable) {
    this.optionalNullable = optionalNullable;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OptionalNullableOnlyModel optionalNullableOnlyModel = (OptionalNullableOnlyModel) o;
    return equalsNullable(this.optionalNullable, optionalNullableOnlyModel.optionalNullable);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(hashCodeNullable(optionalNullable));
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OptionalNullableOnlyModel {\n");
    sb.append("    optionalNullable: ").append(toIndentedString(optionalNullable)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

