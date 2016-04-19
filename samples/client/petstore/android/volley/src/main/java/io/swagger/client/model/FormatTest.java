package io.swagger.client.model;

import java.math.BigDecimal;
import java.util.Date;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class FormatTest  {
  
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
  @SerializedName("password")
  private String password = null;

  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
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
   **/
  @ApiModelProperty(required = true, value = "")
  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
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
  @ApiModelProperty(value = "")
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
  @ApiModelProperty(value = "")
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
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
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
