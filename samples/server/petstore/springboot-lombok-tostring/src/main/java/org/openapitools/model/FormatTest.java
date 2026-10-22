package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.UUID;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * FormatTest
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@JsonTypeName("format_test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class FormatTest {

  @Min(value = 10) @Max(value = 100) 
  @Schema(name = "integer", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer integer;

  @Min(value = 20) @Max(value = 200) 
  @Schema(name = "int32", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("int32")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer int32;

  
  @Schema(name = "int64", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("int64")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Long int64;

  @NotNull @Valid @DecimalMin(value = "32.1") @DecimalMax(value = "543.2") 
  @Schema(name = "number", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private BigDecimal number;

  @DecimalMin(value = "54.3") @DecimalMax(value = "987.6") 
  @Schema(name = "float", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("float")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Float _float;

  @DecimalMin(value = "67.8") @DecimalMax(value = "123.4") 
  @Schema(name = "double", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("double")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Double _double;

  @Pattern(regexp = "[a-zA-Z]") 
  @Schema(name = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String string;

  @NotNull 
  @Schema(name = "byte", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("byte")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private byte[] _byte;

  @Valid 
  @Schema(name = "binary", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("binary")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable org.springframework.core.io.Resource binary;

  @NotNull @Valid 
  @Schema(name = "date", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("date")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate date;

  @Valid 
  @Schema(name = "dateTime", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dateTime")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime dateTime;

  @Valid 
  @Schema(name = "uuid", example = "72f98069-206d-4f12-9f12-3d1e525a8e84", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("uuid")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable UUID uuid;

  @lombok.ToString.Exclude
  @NotNull @Size(min = 10, max = 64) 
  @Schema(name = "password", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("password")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String password;

  @Valid 
  @Schema(name = "BigDecimal", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("BigDecimal")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal bigDecimal;

  public FormatTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FormatTest(BigDecimal number, byte[] _byte, LocalDate date, String password) {
    this.number = number;
    this._byte = _byte;
    this.date = date;
    this.password = password;
  }

  public FormatTest integer(@Nullable Integer integer) {
    this.integer = integer;
    return this;
  }


  public FormatTest int32(@Nullable Integer int32) {
    this.int32 = int32;
    return this;
  }


  public FormatTest int64(@Nullable Long int64) {
    this.int64 = int64;
    return this;
  }


  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }


  public FormatTest _float(@Nullable Float _float) {
    this._float = _float;
    return this;
  }


  public FormatTest _double(@Nullable Double _double) {
    this._double = _double;
    return this;
  }


  public FormatTest string(@Nullable String string) {
    this.string = string;
    return this;
  }


  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }


  public FormatTest binary(@Nullable org.springframework.core.io.Resource binary) {
    this.binary = binary;
    return this;
  }


  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }


  public FormatTest dateTime(@Nullable OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }


  public FormatTest uuid(@Nullable UUID uuid) {
    this.uuid = uuid;
    return this;
  }


  public FormatTest password(String password) {
    this.password = password;
    return this;
  }


  public FormatTest bigDecimal(@Nullable BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
    return this;
  }



}

