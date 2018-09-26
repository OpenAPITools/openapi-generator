package org.openapitools.model;

import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import org.joda.time.LocalDate;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Body3  {
  
  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private Integer integer = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private Integer int32 = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private Long int64 = null;

  @ApiModelProperty(required = true, value = "None")
  @Valid
 /**
   * None
  **/
  private BigDecimal number = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private Float _float = null;

  @ApiModelProperty(required = true, value = "None")
 /**
   * None
  **/
  private Double _double = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private String string = null;

  @ApiModelProperty(required = true, value = "None")
 /**
   * None
  **/
  private String patternWithoutDelimiter = null;

  @ApiModelProperty(required = true, value = "None")
 /**
   * None
  **/
  private byte[] _byte = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private File binary = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private LocalDate date = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private Date dateTime = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private String password = null;

  @ApiModelProperty(value = "None")
 /**
   * None
  **/
  private String callback = null;
 /**
   * None
   * minimum: 10
   * maximum: 100
   * @return integer
  **/
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return integer;
  }

  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  public Body3 integer(Integer integer) {
    this.integer = integer;
    return this;
  }

 /**
   * None
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

  public Body3 int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

 /**
   * None
   * @return int64
  **/
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }

  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  public Body3 int64(Long int64) {
    this.int64 = int64;
    return this;
  }

 /**
   * None
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

  public Body3 number(BigDecimal number) {
    this.number = number;
    return this;
  }

 /**
   * None
   * maximum: 987.6
   * @return _float
  **/
  @JsonProperty("float")
 @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }

  public void setFloat(Float _float) {
    this._float = _float;
  }

  public Body3 _float(Float _float) {
    this._float = _float;
    return this;
  }

 /**
   * None
   * minimum: 67.8
   * maximum: 123.4
   * @return _double
  **/
  @JsonProperty("double")
  @NotNull
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }

  public Body3 _double(Double _double) {
    this._double = _double;
    return this;
  }

 /**
   * None
   * @return string
  **/
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return string;
  }

  public void setString(String string) {
    this.string = string;
  }

  public Body3 string(String string) {
    this.string = string;
    return this;
  }

 /**
   * None
   * @return patternWithoutDelimiter
  **/
  @JsonProperty("pattern_without_delimiter")
  @NotNull
 @Pattern(regexp="^[A-Z].*")  public String getPatternWithoutDelimiter() {
    return patternWithoutDelimiter;
  }

  public void setPatternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
  }

  public Body3 patternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
    return this;
  }

 /**
   * None
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

  public Body3 _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

 /**
   * None
   * @return binary
  **/
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }

  public void setBinary(File binary) {
    this.binary = binary;
  }

  public Body3 binary(File binary) {
    this.binary = binary;
    return this;
  }

 /**
   * None
   * @return date
  **/
  @JsonProperty("date")
  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public Body3 date(LocalDate date) {
    this.date = date;
    return this;
  }

 /**
   * None
   * @return dateTime
  **/
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  public Body3 dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
   * None
   * @return password
  **/
  @JsonProperty("password")
 @Size(min=10,max=64)  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public Body3 password(String password) {
    this.password = password;
    return this;
  }

 /**
   * None
   * @return callback
  **/
  @JsonProperty("callback")
  public String getCallback() {
    return callback;
  }

  public void setCallback(String callback) {
    this.callback = callback;
  }

  public Body3 callback(String callback) {
    this.callback = callback;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body3 {\n");
    
    sb.append("    integer: ").append(toIndentedString(integer)).append("\n");
    sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
    sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
    sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
    sb.append("    patternWithoutDelimiter: ").append(toIndentedString(patternWithoutDelimiter)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    date: ").append(toIndentedString(date)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    callback: ").append(toIndentedString(callback)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

