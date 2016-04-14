package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.Date;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-15T00:36:54.567+08:00")
public class FormatTest  {
  
  private Integer integer = null;
  private Integer int32 = null;
  private Long int64 = null;
  private BigDecimal number = null;
  private Float _float = null;
  private Double _double = null;
  private String string = null;
  private byte[] _byte = null;
  private byte[] binary = null;
  private Date date = null;
  private Date dateTime = null;
  private String password = null;

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("int32")
  public Integer getInt32() {
    return int32;
  }
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }
  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number")
  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("float")
  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("double")
  public Double getDouble() {
    return _double;
  }
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("string")
  public String getString() {
    return string;
  }
  public void setString(String string) {
    this.string = string;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("byte")
  public byte[] getByte() {
    return _byte;
  }
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("binary")
  public byte[] getBinary() {
    return binary;
  }
  public void setBinary(byte[] binary) {
    this.binary = binary;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("date")
  public Date getDate() {
    return date;
  }
  public void setDate(Date date) {
    this.date = date;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }
  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("password")
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
    return Objects.equals(integer, formatTest.integer) &&
        Objects.equals(int32, formatTest.int32) &&
        Objects.equals(int64, formatTest.int64) &&
        Objects.equals(number, formatTest.number) &&
        Objects.equals(_float, formatTest._float) &&
        Objects.equals(_double, formatTest._double) &&
        Objects.equals(string, formatTest.string) &&
        Objects.equals(_byte, formatTest._byte) &&
        Objects.equals(binary, formatTest.binary) &&
        Objects.equals(date, formatTest.date) &&
        Objects.equals(dateTime, formatTest.dateTime) &&
        Objects.equals(password, formatTest.password);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, _byte, binary, date, dateTime, password);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class FormatTest {\n");
    
    sb.append("  integer: ").append(integer).append("\n");
    sb.append("  int32: ").append(int32).append("\n");
    sb.append("  int64: ").append(int64).append("\n");
    sb.append("  number: ").append(number).append("\n");
    sb.append("  _float: ").append(_float).append("\n");
    sb.append("  _double: ").append(_double).append("\n");
    sb.append("  string: ").append(string).append("\n");
    sb.append("  _byte: ").append(_byte).append("\n");
    sb.append("  binary: ").append(binary).append("\n");
    sb.append("  date: ").append(date).append("\n");
    sb.append("  dateTime: ").append(dateTime).append("\n");
    sb.append("  password: ").append(password).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
