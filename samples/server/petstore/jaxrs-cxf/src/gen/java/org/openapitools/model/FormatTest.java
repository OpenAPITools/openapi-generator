package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import java.util.UUID;
import org.joda.time.LocalDate;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class FormatTest  {
  
  @ApiModelProperty(value = "")
  private Integer _integer;

  @ApiModelProperty(value = "")
  private Integer int32;

  @ApiModelProperty(value = "")
  private Long int64;

  @ApiModelProperty(required = true, value = "")
  @Valid
  private BigDecimal number;

  @ApiModelProperty(value = "")
  private Float _float;

  @ApiModelProperty(value = "")
  private Double _double;

  @ApiModelProperty(value = "")
  private String _string;

  @ApiModelProperty(required = true, value = "")
  private byte[] _byte;

  @ApiModelProperty(value = "")
  private File binary;

  @ApiModelProperty(required = true, value = "")
  private LocalDate _date;

  @ApiModelProperty(value = "")
  private Date _dateTime;

  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  private UUID _uuid;

  @ApiModelProperty(required = true, value = "")
  private String password;

  @ApiModelProperty(value = "")
  @Valid
  private BigDecimal _bigDecimal;
 /**
   * Get _integer
   * minimum: 10
   * maximum: 100
   * @return _integer
  **/
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return _integer;
  }

  public void setInteger(Integer _integer) {
    this._integer = _integer;
  }

  public FormatTest _integer(Integer _integer) {
    this._integer = _integer;
    return this;
  }

 /**
   * Get int32
   * minimum: 20
   * maximum: 200
   * @return int32
  **/
  @JsonProperty("int32")
 @Min(20) @Max(200)  public Integer getInt32() {
    return int32;
  }

  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  public FormatTest int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

 /**
   * Get int64
   * @return int64
  **/
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }

  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  public FormatTest int64(Long int64) {
    this.int64 = int64;
    return this;
  }

 /**
   * Get number
   * minimum: 32.1
   * maximum: 543.2
   * @return number
  **/
  @JsonProperty("number")
  @NotNull
 @DecimalMin("32.1") @DecimalMax("543.2")  public BigDecimal getNumber() {
    return number;
  }

  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }

 /**
   * Get _float
   * minimum: 54.3
   * maximum: 987.6
   * @return _float
  **/
  @JsonProperty("float")
 @DecimalMin("54.3") @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }

  public void setFloat(Float _float) {
    this._float = _float;
  }

  public FormatTest _float(Float _float) {
    this._float = _float;
    return this;
  }

 /**
   * Get _double
   * minimum: 67.8
   * maximum: 123.4
   * @return _double
  **/
  @JsonProperty("double")
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }

  public FormatTest _double(Double _double) {
    this._double = _double;
    return this;
  }

 /**
   * Get _string
   * @return _string
  **/
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return _string;
  }

  public void setString(String _string) {
    this._string = _string;
  }

  public FormatTest _string(String _string) {
    this._string = _string;
    return this;
  }

 /**
   * Get _byte
   * @return _byte
  **/
  @JsonProperty("byte")
  @NotNull
  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

 /**
   * Get binary
   * @return binary
  **/
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }

  public void setBinary(File binary) {
    this.binary = binary;
  }

  public FormatTest binary(File binary) {
    this.binary = binary;
    return this;
  }

 /**
   * Get _date
   * @return _date
  **/
  @JsonProperty("date")
  @NotNull
  public LocalDate getDate() {
    return _date;
  }

  public void setDate(LocalDate _date) {
    this._date = _date;
  }

  public FormatTest _date(LocalDate _date) {
    this._date = _date;
    return this;
  }

 /**
   * Get _dateTime
   * @return _dateTime
  **/
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return _dateTime;
  }

  public void setDateTime(Date _dateTime) {
    this._dateTime = _dateTime;
  }

  public FormatTest _dateTime(Date _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

 /**
   * Get _uuid
   * @return _uuid
  **/
  @JsonProperty("uuid")
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  public FormatTest _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

 /**
   * Get password
   * @return password
  **/
  @JsonProperty("password")
  @NotNull
 @Size(min=10,max=64)  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

 /**
   * Get _bigDecimal
   * @return _bigDecimal
  **/
  @JsonProperty("BigDecimal")
  public BigDecimal getBigDecimal() {
    return _bigDecimal;
  }

  public void setBigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
  }

  public FormatTest _bigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
    return this;
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
        Objects.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this._date, formatTest._date) &&
        Objects.equals(this._dateTime, formatTest._dateTime) &&
        Objects.equals(this._uuid, formatTest._uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this._bigDecimal, formatTest._bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_integer, int32, int64, number, _float, _double, _string, _byte, binary, _date, _dateTime, _uuid, password, _bigDecimal);
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
    sb.append("    password: ").append("*").append("\n");
    sb.append("    _bigDecimal: ").append(toIndentedString(_bigDecimal)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

