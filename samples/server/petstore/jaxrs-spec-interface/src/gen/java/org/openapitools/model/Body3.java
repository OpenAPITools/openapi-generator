package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import org.joda.time.LocalDate;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class Body3  implements Serializable {
  
  private @Valid Integer integer = null;
  private @Valid Integer int32 = null;
  private @Valid Long int64 = null;
  private @Valid BigDecimal number = null;
  private @Valid Float _float = null;
  private @Valid Double _double = null;
  private @Valid String string = null;
  private @Valid String patternWithoutDelimiter = null;
  private @Valid byte[] _byte = null;
  private @Valid File binary = null;
  private @Valid LocalDate date = null;
  private @Valid Date dateTime = null;
  private @Valid String password = null;
  private @Valid String callback = null;

  /**
   * None
   * minimum: 10
   * maximum: 100
   **/
  public Body3 integer(Integer integer) {
    this.integer = integer;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   * None
   * minimum: 20
   * maximum: 200
   **/
  public Body3 int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("int32")
 @Min(20) @Max(200)  public Integer getInt32() {
    return int32;
  }
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   * None
   **/
  public Body3 int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }
  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  /**
   * None
   * minimum: 32.1
   * maximum: 543.2
   **/
  public Body3 number(BigDecimal number) {
    this.number = number;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "None")
  @JsonProperty("number")
  @NotNull
 @DecimalMin("32.1") @DecimalMax("543.2")  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   * None
   * maximum: 987.6
   **/
  public Body3 _float(Float _float) {
    this._float = _float;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("float")
 @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   * None
   * minimum: 67.8
   * maximum: 123.4
   **/
  public Body3 _double(Double _double) {
    this._double = _double;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "None")
  @JsonProperty("double")
  @NotNull
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   * None
   **/
  public Body3 string(String string) {
    this.string = string;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return string;
  }
  public void setString(String string) {
    this.string = string;
  }

  /**
   * None
   **/
  public Body3 patternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "None")
  @JsonProperty("pattern_without_delimiter")
  @NotNull
 @Pattern(regexp="^[A-Z].*")  public String getPatternWithoutDelimiter() {
    return patternWithoutDelimiter;
  }
  public void setPatternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
  }

  /**
   * None
   **/
  public Body3 _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "None")
  @JsonProperty("byte")
  @NotNull
  public byte[] getByte() {
    return _byte;
  }
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   * None
   **/
  public Body3 binary(File binary) {
    this.binary = binary;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }
  public void setBinary(File binary) {
    this.binary = binary;
  }

  /**
   * None
   **/
  public Body3 date(LocalDate date) {
    this.date = date;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("date")
  public LocalDate getDate() {
    return date;
  }
  public void setDate(LocalDate date) {
    this.date = date;
  }

  /**
   * None
   **/
  public Body3 dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }
  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   * None
   **/
  public Body3 password(String password) {
    this.password = password;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("password")
 @Size(min=10,max=64)  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
  }

  /**
   * None
   **/
  public Body3 callback(String callback) {
    this.callback = callback;
    return this;
  }

  
  @ApiModelProperty(value = "None")
  @JsonProperty("callback")
  public String getCallback() {
    return callback;
  }
  public void setCallback(String callback) {
    this.callback = callback;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body3 body3 = (Body3) o;
    return Objects.equals(integer, body3.integer) &&
        Objects.equals(int32, body3.int32) &&
        Objects.equals(int64, body3.int64) &&
        Objects.equals(number, body3.number) &&
        Objects.equals(_float, body3._float) &&
        Objects.equals(_double, body3._double) &&
        Objects.equals(string, body3.string) &&
        Objects.equals(patternWithoutDelimiter, body3.patternWithoutDelimiter) &&
        Objects.equals(_byte, body3._byte) &&
        Objects.equals(binary, body3.binary) &&
        Objects.equals(date, body3.date) &&
        Objects.equals(dateTime, body3.dateTime) &&
        Objects.equals(password, body3.password) &&
        Objects.equals(callback, body3.callback);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, patternWithoutDelimiter, _byte, binary, date, dateTime, password, callback);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

