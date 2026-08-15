package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
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
 * NullableModel
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.25.0-SNAPSHOT")
public class NullableModel implements Serializable {

  private static final long serialVersionUID = 1L;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String requiredNonNullable;

  @JsonInclude(JsonInclude.Include.ALWAYS)
  private JsonNullable<String> requiredNullable = JsonNullable.<String>undefined();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String optionalNonNullable;

  private JsonNullable<String> optionalNullable = JsonNullable.<String>undefined();

  public NullableModel() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public NullableModel(String requiredNonNullable, String requiredNullable) {
    this.requiredNonNullable = requiredNonNullable;
    this.requiredNullable = JsonNullable.of(requiredNullable);
  }

  public NullableModel requiredNonNullable(String requiredNonNullable) {
    this.requiredNonNullable = requiredNonNullable;
    return this;
  }

  /**
   * Get requiredNonNullable
   * @return requiredNonNullable
   */
  @NotNull 
  @JsonProperty("requiredNonNullable")
  public String getRequiredNonNullable() {
    return requiredNonNullable;
  }

  @JsonProperty("requiredNonNullable")
  public void setRequiredNonNullable(String requiredNonNullable) {
    this.requiredNonNullable = requiredNonNullable;
  }

  public NullableModel requiredNullable(String requiredNullable) {
    this.requiredNullable = JsonNullable.of(requiredNullable);
    return this;
  }

  /**
   * Get requiredNullable
   * @return requiredNullable
   */
  @NotNull 
  @JsonProperty("requiredNullable")
  public JsonNullable<String> getRequiredNullable() {
    return requiredNullable;
  }

  @JsonProperty("requiredNullable")
  public void setRequiredNullable(JsonNullable<String> requiredNullable) {
    this.requiredNullable = requiredNullable;
  }

  public NullableModel optionalNonNullable(@Nullable String optionalNonNullable) {
    this.optionalNonNullable = optionalNonNullable;
    return this;
  }

  /**
   * Get optionalNonNullable
   * @return optionalNonNullable
   */
  
  @JsonProperty("optionalNonNullable")
  public @Nullable String getOptionalNonNullable() {
    return optionalNonNullable;
  }

  @JsonProperty("optionalNonNullable")
  public void setOptionalNonNullable(@Nullable String optionalNonNullable) {
    this.optionalNonNullable = optionalNonNullable;
  }

  public NullableModel optionalNullable(String optionalNullable) {
    this.optionalNullable = JsonNullable.of(optionalNullable);
    return this;
  }

  /**
   * Get optionalNullable
   * @return optionalNullable
   */
  
  @JsonProperty("optionalNullable")
  public JsonNullable<String> getOptionalNullable() {
    return optionalNullable;
  }

  public void setOptionalNullable(JsonNullable<String> optionalNullable) {
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
    NullableModel nullableModel = (NullableModel) o;
    return Objects.equals(this.requiredNonNullable, nullableModel.requiredNonNullable) &&
        Objects.equals(this.requiredNullable, nullableModel.requiredNullable) &&
        Objects.equals(this.optionalNonNullable, nullableModel.optionalNonNullable) &&
        equalsNullable(this.optionalNullable, nullableModel.optionalNullable);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(requiredNonNullable, requiredNullable, optionalNonNullable, hashCodeNullable(optionalNullable));
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
    sb.append("class NullableModel {\n");
    sb.append("    requiredNonNullable: ").append(toIndentedString(requiredNonNullable)).append("\n");
    sb.append("    requiredNullable: ").append(toIndentedString(requiredNullable)).append("\n");
    sb.append("    optionalNonNullable: ").append(toIndentedString(optionalNonNullable)).append("\n");
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

