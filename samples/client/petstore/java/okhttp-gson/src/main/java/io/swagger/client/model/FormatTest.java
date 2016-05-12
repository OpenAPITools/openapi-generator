package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.Date;

import com.google.gson.annotations.SerializedName;


/**
 * FormatTest
 */
public class FormatTest   {
  
  @SerializedName("integer")
  private Integer integer = null;

  @SerializedName("int32")
  private Integer int32 = null;

  @SerializedName("int64")
  private Long int64 = null;

  @SerializedName("number")
  private BigDecimal number = null;

  @SerializedName("float")
  private Float _float = null;

  @SerializedName("double")
  private Double _double = null;

  @SerializedName("string")
  private String string = null;

  @SerializedName("byte")
  private byte[] _byte = null;

  @SerializedName("binary")
  private byte[] binary = null;

  @SerializedName("date")
  private Date date = null;

  @SerializedName("dateTime")
  private Date dateTime = null;

  @SerializedName("uuid")
  private String uuid = null;

  @SerializedName("password")
  private String password = null;

  /**
   * minimum: 10.0
   * maximum: 100.0
   **/
  @ApiModelProperty(value = "")
  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   * minimum: 20.0
   * maximum: 200.0
   **/
  @ApiModelProperty(value = "")
  public Integer getInt32() {
    return int32;
  }
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Long getInt64() {
    return int64;
  }
  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  /**
   * minimum: 32.1
   * maximum: 543.2
   **/
  @ApiModelProperty(required = true, value = "")
  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   * minimum: 54.3
   * maximum: 987.6
   **/
  @ApiModelProperty(value = "")
  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   * minimum: 67.8
   * maximum: 123.4
   **/
  @ApiModelProperty(value = "")
  public Double getDouble() {
    return _double;
  }
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public String getString() {
    return string;
  }
  public void setString(String string) {
    this.string = string;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public byte[] getByte() {
    return _byte;
  }
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public byte[] getBinary() {
    return binary;
  }
  public void setBinary(byte[] binary) {
    this.binary = binary;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public Date getDate() {
    return date;
  }
  public void setDate(Date date) {
    this.date = date;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Date getDateTime() {
    return dateTime;
  }
  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public String getUuid() {
    return uuid;
  }
  public void setUuid(String uuid) {
    this.uuid = uuid;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
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
        Objects.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this.date, formatTest.date) &&
        Objects.equals(this.dateTime, formatTest.dateTime) &&
        Objects.equals(this.uuid, formatTest.uuid) &&
        Objects.equals(this.password, formatTest.password);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, _byte, binary, date, dateTime, uuid, password);
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

