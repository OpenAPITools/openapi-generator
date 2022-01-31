package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.UUID;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * FormatTest
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class FormatTest   {

  @JsonProperty("integer")
  private Integer integer;

  @JsonProperty("int32")
  private Integer int32;

  @JsonProperty("int64")
  private Long int64;

  @JsonProperty("number")
  private BigDecimal number;

  @JsonProperty("float")
  private Float _float;

  @JsonProperty("double")
  private Double _double;

  @JsonProperty("string")
  private String string;

  @JsonProperty("byte")
  private byte[] _byte;

  @JsonProperty("binary")
  private org.springframework.core.io.Resource binary;

  @JsonProperty("date")
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate date;

  @JsonProperty("dateTime")
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime dateTime;

  @JsonProperty("uuid")
  private UUID uuid;

  @JsonProperty("password")
  private String password;

  @JsonProperty("BigDecimal")
  private BigDecimal bigDecimal;

  public FormatTest integer(Integer integer) {
    this.integer = integer;
    return this;
  }

  /**
   * Get integer
   * minimum: 10
   * maximum: 100
   * @return integer
  */
  @Min(10) @Max(100) 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "integer", required = false)
  public Optional<@Min(10) @Max(100) Integer> getInteger() {
    return Optional.ofNullable(integer);
  }

  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  public FormatTest int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  /**
   * Get int32
   * minimum: 20
   * maximum: 200
   * @return int32
  */
  @Min(20) @Max(200) 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "int32", required = false)
  public Optional<@Min(20) @Max(200) Integer> getInt32() {
    return Optional.ofNullable(int32);
  }

  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  public FormatTest int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  /**
   * Get int64
   * @return int64
  */
  
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "int64", required = false)
  public Optional<Long> getInt64() {
    return Optional.ofNullable(int64);
  }

  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }

  /**
   * Get number
   * minimum: 32.1
   * maximum: 543.2
   * @return number
  */
  @NotNull @Valid @DecimalMin("32.1") @DecimalMax("543.2") 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "number", required = true)
  public BigDecimal getNumber() {
    return number;
  }

  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  public FormatTest _float(Float _float) {
    this._float = _float;
    return this;
  }

  /**
   * Get _float
   * minimum: 54.3
   * maximum: 987.6
   * @return _float
  */
  @DecimalMin("54.3") @DecimalMax("987.6") 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "float", required = false)
  public Optional<@DecimalMin("54.3") @DecimalMax("987.6") Float> getFloat() {
    return Optional.ofNullable(_float);
  }

  public void setFloat(Float _float) {
    this._float = _float;
  }

  public FormatTest _double(Double _double) {
    this._double = _double;
    return this;
  }

  /**
   * Get _double
   * minimum: 67.8
   * maximum: 123.4
   * @return _double
  */
  @DecimalMin("67.8") @DecimalMax("123.4") 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "double", required = false)
  public Optional<@DecimalMin("67.8") @DecimalMax("123.4") Double> getDouble() {
    return Optional.ofNullable(_double);
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }

  public FormatTest string(String string) {
    this.string = string;
    return this;
  }

  /**
   * Get string
   * @return string
  */
  @Pattern(regexp = "/[a-z]/i") 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "string", required = false)
  public Optional<@Pattern(regexp = "/[a-z]/i") String> getString() {
    return Optional.ofNullable(string);
  }

  public void setString(String string) {
    this.string = string;
  }

  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  /**
   * Get _byte
   * @return _byte
  */
  @NotNull 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "byte", required = true)
  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public FormatTest binary(org.springframework.core.io.Resource binary) {
    this.binary = binary;
    return this;
  }

  /**
   * Get binary
   * @return binary
  */
  @Valid 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "binary", required = false)
  public Optional<org.springframework.core.io.Resource> getBinary() {
    return Optional.ofNullable(binary);
  }

  public void setBinary(org.springframework.core.io.Resource binary) {
    this.binary = binary;
  }

  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }

  /**
   * Get date
   * @return date
  */
  @NotNull @Valid 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "date", required = true)
  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public FormatTest dateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
  */
  @Valid 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "dateTime", required = false)
  public Optional<OffsetDateTime> getDateTime() {
    return Optional.ofNullable(dateTime);
  }

  public void setDateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

  /**
   * Get uuid
   * @return uuid
  */
  @Valid 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "uuid", example = "72f98069-206d-4f12-9f12-3d1e525a8e84", required = false)
  public Optional<UUID> getUuid() {
    return Optional.ofNullable(uuid);
  }

  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

  /**
   * Get password
   * @return password
  */
  @NotNull @Size(min = 10, max = 64) 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "password", required = true)
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest bigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
    return this;
  }

  /**
   * Get bigDecimal
   * @return bigDecimal
  */
  @Valid 
  // Rely on the @JsonProperty annotation on the variable and ignore the getter methods.
  @JsonIgnore
  @Schema(name = "BigDecimal", required = false)
  public Optional<BigDecimal> getBigDecimal() {
    return Optional.ofNullable(bigDecimal);
  }

  public void setBigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FormatTest formatTest = (FormatTest) o;
    return Objects.equals(this.integer, formatTest.integer) &&
        Objects.equals(this.int32, formatTest.int32) &&
        Objects.equals(this.int64, formatTest.int64) &&
        Objects.equals(this.number, formatTest.number) &&
        Objects.equals(this._float, formatTest._float) &&
        Objects.equals(this._double, formatTest._double) &&
        Objects.equals(this.string, formatTest.string) &&
        Arrays.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this.date, formatTest.date) &&
        Objects.equals(this.dateTime, formatTest.dateTime) &&
        Objects.equals(this.uuid, formatTest.uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this.bigDecimal, formatTest.bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, Arrays.hashCode(_byte), binary, date, dateTime, uuid, password, bigDecimal);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FormatTest {\n");
    sb.append("    integer: ").append(toIndentedString(integer)).append("\n");
    sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
    sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
    sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    date: ").append(toIndentedString(date)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    bigDecimal: ").append(toIndentedString(bigDecimal)).append("\n");
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

