package org.openapitools.virtualan.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.UUID;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * FormatTest
 */

@JsonTypeName("format_test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class FormatTest {

  private Integer _integer;

  private Integer int32;

  private Long int64;

  private BigDecimal number;

  private Float _float;

  private Double _double;

  private String _string;

  private byte[] _byte;

  private org.springframework.core.io.Resource binary;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate _date;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime _dateTime;

  private UUID _uuid;

  private String password;

  private BigDecimal _bigDecimal;

  public FormatTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FormatTest(BigDecimal number, byte[] _byte, LocalDate _date, String password) {
    this.number = number;
    this._byte = _byte;
    this._date = _date;
    this.password = password;
  }

  public FormatTest _integer(Integer _integer) {
    this._integer = _integer;
    return this;
  }

  /**
   * Get _integer
   * minimum: 10
   * maximum: 100
   * @return _integer
  */
  @Min(10) @Max(100) 
  @Schema(name = "integer", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("integer")
  public Integer getInteger() {
    return _integer;
  }

  public void setInteger(Integer _integer) {
    this._integer = _integer;
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
  @Schema(name = "int32", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("int32")
  public Integer getInt32() {
    return int32;
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
  
  @Schema(name = "int64", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
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
  @Schema(name = "number", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("number")
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
  @Schema(name = "float", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("float")
  public Float getFloat() {
    return _float;
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
  @Schema(name = "double", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("double")
  public Double getDouble() {
    return _double;
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }

  public FormatTest _string(String _string) {
    this._string = _string;
    return this;
  }

  /**
   * Get _string
   * @return _string
  */
  @Pattern(regexp = "/[a-z]/i") 
  @Schema(name = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("string")
  public String getString() {
    return _string;
  }

  public void setString(String _string) {
    this._string = _string;
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
  @Schema(name = "byte", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("byte")
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
  @Schema(name = "binary", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("binary")
  public org.springframework.core.io.Resource getBinary() {
    return binary;
  }

  public void setBinary(org.springframework.core.io.Resource binary) {
    this.binary = binary;
  }

  public FormatTest _date(LocalDate _date) {
    this._date = _date;
    return this;
  }

  /**
   * Get _date
   * @return _date
  */
  @NotNull @Valid 
  @Schema(name = "date", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("date")
  public LocalDate getDate() {
    return _date;
  }

  public void setDate(LocalDate _date) {
    this._date = _date;
  }

  public FormatTest _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

  /**
   * Get _dateTime
   * @return _dateTime
  */
  @Valid 
  @Schema(name = "dateTime", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dateTime")
  public OffsetDateTime getDateTime() {
    return _dateTime;
  }

  public void setDateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
  }

  public FormatTest _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

  /**
   * Get _uuid
   * @return _uuid
  */
  @Valid 
  @Schema(name = "uuid", example = "72f98069-206d-4f12-9f12-3d1e525a8e84", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("uuid")
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
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
  @Schema(name = "password", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest _bigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
    return this;
  }

  /**
   * Get _bigDecimal
   * @return _bigDecimal
  */
  @Valid 
  @Schema(name = "BigDecimal", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("BigDecimal")
  public BigDecimal getBigDecimal() {
    return _bigDecimal;
  }

  public void setBigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
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
    return Objects.equals(this._integer, formatTest._integer) &&
        Objects.equals(this.int32, formatTest.int32) &&
        Objects.equals(this.int64, formatTest.int64) &&
        Objects.equals(this.number, formatTest.number) &&
        Objects.equals(this._float, formatTest._float) &&
        Objects.equals(this._double, formatTest._double) &&
        Objects.equals(this._string, formatTest._string) &&
        Arrays.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this._date, formatTest._date) &&
        Objects.equals(this._dateTime, formatTest._dateTime) &&
        Objects.equals(this._uuid, formatTest._uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this._bigDecimal, formatTest._bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_integer, int32, int64, number, _float, _double, _string, Arrays.hashCode(_byte), binary, _date, _dateTime, _uuid, password, _bigDecimal);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FormatTest {\n");
    sb.append("    _integer: ").append(toIndentedString(_integer)).append("\n");
    sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
    sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
    sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
    sb.append("    _string: ").append(toIndentedString(_string)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    _date: ").append(toIndentedString(_date)).append("\n");
    sb.append("    _dateTime: ").append(toIndentedString(_dateTime)).append("\n");
    sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    _bigDecimal: ").append(toIndentedString(_bigDecimal)).append("\n");
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

