package io.swagger.model;

import java.math.BigDecimal;
import java.util.UUID;
import org.joda.time.LocalDate;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class FormatTest  {
  
  @ApiModelProperty(example = "null", value = "")
  private Integer integer = null;
  @ApiModelProperty(example = "null", value = "")
  private Integer int32 = null;
  @ApiModelProperty(example = "null", value = "")
  private Long int64 = null;
  @ApiModelProperty(example = "null", required = true, value = "")
  private BigDecimal number = null;
  @ApiModelProperty(example = "null", value = "")
  private Float _float = null;
  @ApiModelProperty(example = "null", value = "")
  private Double _double = null;
  @ApiModelProperty(example = "null", value = "")
  private String string = null;
  @ApiModelProperty(example = "null", required = true, value = "")
  private byte[] _byte = null;
  @ApiModelProperty(example = "null", value = "")
  private byte[] binary = null;
  @ApiModelProperty(example = "null", required = true, value = "")
  private LocalDate date = null;
  @ApiModelProperty(example = "null", value = "")
  private javax.xml.datatype.XMLGregorianCalendar dateTime = null;
  @ApiModelProperty(example = "null", value = "")
  private UUID uuid = null;
  @ApiModelProperty(example = "null", required = true, value = "")
  private String password = null;

 /**
   * Get integer
   * minimum: 10
   * maximum: 100
   * @return integer
  **/
  public Integer getInteger() {
    return integer;
  }

  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  public FormatTest integer(Integer integer) {
    this.integer = integer;
    return this;
  }

 /**
   * Get int32
   * minimum: 20
   * maximum: 200
   * @return int32
  **/
  public Integer getInt32() {
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
  public BigDecimal getNumber() {
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
  public Float getFloat() {
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
  public Double getDouble() {
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
   * Get string
   * @return string
  **/
  public String getString() {
    return string;
  }

  public void setString(String string) {
    this.string = string;
  }

  public FormatTest string(String string) {
    this.string = string;
    return this;
  }

 /**
   * Get _byte
   * @return _byte
  **/
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
  public byte[] getBinary() {
    return binary;
  }

  public void setBinary(byte[] binary) {
    this.binary = binary;
  }

  public FormatTest binary(byte[] binary) {
    this.binary = binary;
    return this;
  }

 /**
   * Get date
   * @return date
  **/
  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }

 /**
   * Get dateTime
   * @return dateTime
  **/
  public javax.xml.datatype.XMLGregorianCalendar getDateTime() {
    return dateTime;
  }

  public void setDateTime(javax.xml.datatype.XMLGregorianCalendar dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest dateTime(javax.xml.datatype.XMLGregorianCalendar dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
   * Get uuid
   * @return uuid
  **/
  public UUID getUuid() {
    return uuid;
  }

  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  public FormatTest uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

 /**
   * Get password
   * @return password
  **/
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest password(String password) {
    this.password = password;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

