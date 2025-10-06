package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.UUID;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * FormatTest
 */

@JsonTypeName("format_test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class FormatTest {

  private @Nullable Integer integer;

  private @Nullable Integer int32;

  private @Nullable Long int64;

  private BigDecimal number;

  private @Nullable Float _float;

  private @Nullable Double _double;

  private @Nullable String string;

  private byte[] _byte;

  private @Nullable org.springframework.core.io.Resource binary;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate date;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime dateTime;

  private @Nullable UUID uuid;

  private String password;

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

  /**
   * Constructor with all args parameters
   */
  public FormatTest(@Nullable Integer integer, @Nullable Integer int32, @Nullable Long int64, BigDecimal number, @Nullable Float _float, @Nullable Double _double, @Nullable String string, byte[] _byte, @Nullable org.springframework.core.io.Resource binary, LocalDate date, @Nullable OffsetDateTime dateTime, @Nullable UUID uuid, String password, @Nullable BigDecimal bigDecimal) {
      this.integer = integer;
      this.int32 = int32;
      this.int64 = int64;
      this.number = number;
      this._float = _float;
      this._double = _double;
      this.string = string;
      this._byte = _byte;
      this.binary = binary;
      this.date = date;
      this.dateTime = dateTime;
      this.uuid = uuid;
      this.password = password;
      this.bigDecimal = bigDecimal;
  }

  public FormatTest integer(@Nullable Integer integer) {
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
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
  public @Nullable Integer getInteger() {
    return integer;
  }

  public void setInteger(@Nullable Integer integer) {
    this.integer = integer;
  }

  public FormatTest int32(@Nullable Integer int32) {
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
  @ApiModelProperty(value = "")
  @JsonProperty("int32")
  public @Nullable Integer getInt32() {
    return int32;
  }

  public void setInt32(@Nullable Integer int32) {
    this.int32 = int32;
  }

  public FormatTest int64(@Nullable Long int64) {
    this.int64 = int64;
    return this;
  }

  /**
   * Get int64
   * @return int64
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("int64")
  public @Nullable Long getInt64() {
    return int64;
  }

  public void setInt64(@Nullable Long int64) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number")
  public BigDecimal getNumber() {
    return number;
  }

  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  public FormatTest _float(@Nullable Float _float) {
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
  @ApiModelProperty(value = "")
  @JsonProperty("float")
  public @Nullable Float getFloat() {
    return _float;
  }

  public void setFloat(@Nullable Float _float) {
    this._float = _float;
  }

  public FormatTest _double(@Nullable Double _double) {
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
  @ApiModelProperty(value = "")
  @JsonProperty("double")
  public @Nullable Double getDouble() {
    return _double;
  }

  public void setDouble(@Nullable Double _double) {
    this._double = _double;
  }

  public FormatTest string(@Nullable String string) {
    this.string = string;
    return this;
  }

  /**
   * Get string
   * @return string
   */
  @Pattern(regexp = "/[a-z]/i") 
  @ApiModelProperty(value = "")
  @JsonProperty("string")
  public @Nullable String getString() {
    return string;
  }

  public void setString(@Nullable String string) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("byte")
  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public FormatTest binary(@Nullable org.springframework.core.io.Resource binary) {
    this.binary = binary;
    return this;
  }

  /**
   * Get binary
   * @return binary
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("binary")
  public @Nullable org.springframework.core.io.Resource getBinary() {
    return binary;
  }

  public void setBinary(@Nullable org.springframework.core.io.Resource binary) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("date")
  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public FormatTest dateTime(@Nullable OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public @Nullable OffsetDateTime getDateTime() {
    return dateTime;
  }

  public void setDateTime(@Nullable OffsetDateTime dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest uuid(@Nullable UUID uuid) {
    this.uuid = uuid;
    return this;
  }

  /**
   * Get uuid
   * @return uuid
   */
  @Valid 
  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  @JsonProperty("uuid")
  public @Nullable UUID getUuid() {
    return uuid;
  }

  public void setUuid(@Nullable UUID uuid) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest bigDecimal(@Nullable BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
    return this;
  }

  /**
   * Get bigDecimal
   * @return bigDecimal
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("BigDecimal")
  public @Nullable BigDecimal getBigDecimal() {
    return bigDecimal;
  }

  public void setBigDecimal(@Nullable BigDecimal bigDecimal) {
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
    sb.append("    password: ").append("*").append("\n");
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

