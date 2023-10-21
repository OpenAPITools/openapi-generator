package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import java.util.UUID;

/**
 * FormatTest
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class FormatTest   {
  @JsonProperty("integer")
  private Integer _integer;

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
  private String _string;

  @JsonProperty("byte")
  private byte[] _byte;

  @JsonProperty("binary")
  private File binary;

  @JsonProperty("date")
  private Date _date;

  @JsonProperty("dateTime")
  private Date _dateTime;

  @JsonProperty("uuid")
  private UUID _uuid;

  @JsonProperty("password")
  private String password;

  @JsonProperty("BigDecimal")
  private BigDecimal _bigDecimal;

  public FormatTest _integer(Integer _integer) {
    this._integer = _integer;
    return this;
  }

   /**
   * Get _integer
   * minimum: 10
   * maximum: 100
   * @return _integer
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(required = true, value = "")
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
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(value = "")
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
  **/
  @ApiModelProperty(required = true, value = "")
  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public FormatTest binary(File binary) {
    this.binary = binary;
    return this;
  }

   /**
   * Get binary
   * @return binary
  **/
  @ApiModelProperty(value = "")
  public File getBinary() {
    return binary;
  }

  public void setBinary(File binary) {
    this.binary = binary;
  }

  public FormatTest _date(Date _date) {
    this._date = _date;
    return this;
  }

   /**
   * Get _date
   * @return _date
  **/
  @ApiModelProperty(required = true, value = "")
  public Date getDate() {
    return _date;
  }

  public void setDate(Date _date) {
    this._date = _date;
  }

  public FormatTest _dateTime(Date _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

   /**
   * Get _dateTime
   * @return _dateTime
  **/
  @ApiModelProperty(value = "")
  public Date getDateTime() {
    return _dateTime;
  }

  public void setDateTime(Date _dateTime) {
    this._dateTime = _dateTime;
  }

  public FormatTest _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

   /**
   * Get _uuid
   * @return _uuid
  **/
  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
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
  **/
  @ApiModelProperty(required = true, value = "")
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
  **/
  @ApiModelProperty(value = "")
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

