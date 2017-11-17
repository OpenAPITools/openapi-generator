package io.swagger.model;

import java.math.BigDecimal;
import java.util.Date;
import java.util.UUID;
import org.joda.time.LocalDate;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class FormatTest  implements Serializable {
  
  private @Valid Integer integer = null;
  private @Valid Integer int32 = null;
  private @Valid Long int64 = null;
  private @Valid BigDecimal number = null;
  private @Valid Float _float = null;
  private @Valid Double _double = null;
  private @Valid String string = null;
  private @Valid byte[] _byte = null;
  private @Valid byte[] binary = null;
  private @Valid LocalDate date = null;
  private @Valid Date dateTime = null;
  private @Valid UUID uuid = null;
  private @Valid String password = null;

  /**
   * minimum: 10
   * maximum: 100
   **/
  public FormatTest integer(Integer integer) {
    this.integer = integer;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   * minimum: 20
   * maximum: 200
   **/
  public FormatTest int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("int32")
 @Min(20) @Max(200)  public Integer getInt32() {
    return int32;
  }
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   **/
  public FormatTest int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("int64")
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
  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number")
  @NotNull
 @DecimalMin("32.1") @DecimalMax("543.2")  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   * minimum: 54.3
   * maximum: 987.6
   **/
  public FormatTest _float(Float _float) {
    this._float = _float;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("float")
 @DecimalMin("54.3") @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   * minimum: 67.8
   * maximum: 123.4
   **/
  public FormatTest _double(Double _double) {
    this._double = _double;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("double")
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   **/
  public FormatTest string(String string) {
    this.string = string;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return string;
  }
  public void setString(String string) {
    this.string = string;
  }

  /**
   **/
  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("byte")
  @NotNull
 @Pattern(regexp="^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$")  public byte[] getByte() {
    return _byte;
  }
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   **/
  public FormatTest binary(byte[] binary) {
    this.binary = binary;
    return this;
  }

  
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
  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("date")
  @NotNull
  public LocalDate getDate() {
    return date;
  }
  public void setDate(LocalDate date) {
    this.date = date;
  }

  /**
   **/
  public FormatTest dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  
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
  public FormatTest uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }
  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  /**
   **/
  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("password")
  @NotNull
 @Size(min=10,max=64)  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
        Objects.equals(uuid, formatTest.uuid) &&
        Objects.equals(password, formatTest.password);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

