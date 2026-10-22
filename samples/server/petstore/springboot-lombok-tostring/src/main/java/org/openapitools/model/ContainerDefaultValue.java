package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ContainerDefaultValue
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class ContainerDefaultValue {

  
  @Schema(name = "nullable_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
  @JsonProperty("nullable_array")
  private JsonNullable<List<String>> nullableArray = JsonNullable.<List<String>>undefined();

  /* @Present */ 
  @Schema(name = "nullable_required_array", requiredMode = Schema.RequiredMode.REQUIRED, nullable = true)
  @JsonProperty("nullable_required_array")
  @JsonInclude(JsonInclude.Include.ALWAYS)
  private JsonNullable<List<String>> nullableRequiredArray = JsonNullable.<List<String>>undefined();

  @NotNull 
  @Schema(name = "required_array", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("required_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<String> requiredArray = new ArrayList<>();

  
  @Schema(name = "nullable_array_with_default", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
  @JsonProperty("nullable_array_with_default")
  private JsonNullable<List<String>> nullableArrayWithDefault = JsonNullable.<List<String>>undefined();

  public ContainerDefaultValue() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public ContainerDefaultValue(List<String> nullableRequiredArray, List<String> requiredArray) {
    this.nullableRequiredArray = JsonNullable.of(nullableRequiredArray);
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValue nullableArray(List<String> nullableArray) {
    this.nullableArray = JsonNullable.of(nullableArray);
    return this;
  }

  public ContainerDefaultValue addNullableArrayItem(String nullableArrayItem) {
    if (this.nullableArray == null || !this.nullableArray.isPresent() || this.nullableArray.get() == null) {
      this.nullableArray = JsonNullable.of(new ArrayList<>());
    }
    this.nullableArray.get().add(nullableArrayItem);
    return this;
  }


  public ContainerDefaultValue nullableRequiredArray(List<String> nullableRequiredArray) {
    this.nullableRequiredArray = JsonNullable.of(nullableRequiredArray);
    return this;
  }

  public ContainerDefaultValue addNullableRequiredArrayItem(String nullableRequiredArrayItem) {
    if (this.nullableRequiredArray == null || !this.nullableRequiredArray.isPresent() || this.nullableRequiredArray.get() == null) {
      this.nullableRequiredArray = JsonNullable.of(new ArrayList<>());
    }
    this.nullableRequiredArray.get().add(nullableRequiredArrayItem);
    return this;
  }


  public ContainerDefaultValue requiredArray(List<String> requiredArray) {
    this.requiredArray = requiredArray;
    return this;
  }

  public ContainerDefaultValue addRequiredArrayItem(String requiredArrayItem) {
    if (this.requiredArray == null) {
      this.requiredArray = new ArrayList<>();
    }
    this.requiredArray.add(requiredArrayItem);
    return this;
  }


  public ContainerDefaultValue nullableArrayWithDefault(List<String> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = JsonNullable.of(nullableArrayWithDefault);
    return this;
  }

  public ContainerDefaultValue addNullableArrayWithDefaultItem(String nullableArrayWithDefaultItem) {
    if (this.nullableArrayWithDefault == null || !this.nullableArrayWithDefault.isPresent() || this.nullableArrayWithDefault.get() == null) {
      this.nullableArrayWithDefault = JsonNullable.of(new ArrayList<>());
    }
    this.nullableArrayWithDefault.get().add(nullableArrayWithDefaultItem);
    return this;
  }



}

