package org.openapitools.model;

import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import java.util.UUID;
import org.joda.time.LocalDate;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class FormatTest  {
  
  @ApiModelProperty(value = "")
  private Integer integer;

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
  private String string;

  @ApiModelProperty(required = true, value = "")
  private byte[] _byte;

  @ApiModelProperty(value = "")
  private File binary;

  @ApiModelProperty(required = true, value = "")
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
  private LocalDate date;

  @ApiModelProperty(value = "")
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'hh:mm:ss.SSSX")
  private Date dateTime;

  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  private UUID uuid;

  @ApiModelProperty(required = true, value = "")
  private String password;

  @ApiModelProperty(value = "")
  @Valid
  private BigDecimal bigDecimal;
 /**
  * Get integer
  * minimum: 10
  * maximum: 100
  * @return integer
  */
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return integer;
  }

  /**
   * Sets the <code>integer</code> property.
   */
 public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   * Sets the <code>integer</code> property.
   */
  public FormatTest integer(Integer integer) {
    this.integer = integer;
    return this;
  }

 /**
  * Get int32
  * minimum: 20
  * maximum: 200
  * @return int32
  */
  @JsonProperty("int32")
 @Min(20) @Max(200)  public Integer getInt32() {
    return int32;
  }

  /**
   * Sets the <code>int32</code> property.
   */
 public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   * Sets the <code>int32</code> property.
   */
  public FormatTest int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

 /**
  * Get int64
  * @return int64
  */
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }

  /**
   * Sets the <code>int64</code> property.
   */
 public void setInt64(Long int64) {
    this.int64 = int64;
  }

  /**
   * Sets the <code>int64</code> property.
   */
  public FormatTest int64(Long int64) {
    this.int64 = int64;
    return this;
  }

 /**
  * Get number
  * minimum: 32.1
  * maximum: 543.2
  * @return number
  */
  @JsonProperty("number")
  @NotNull
 @DecimalMin("32.1") @DecimalMax("543.2")  public BigDecimal getNumber() {
    return number;
  }

  /**
   * Sets the <code>number</code> property.
   */
 public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   * Sets the <code>number</code> property.
   */
  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }

 /**
  * Get _float
  * minimum: 54.3
  * maximum: 987.6
  * @return _float
  */
  @JsonProperty("float")
 @DecimalMin("54.3") @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }

  /**
   * Sets the <code>_float</code> property.
   */
 public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   * Sets the <code>_float</code> property.
   */
  public FormatTest _float(Float _float) {
    this._float = _float;
    return this;
  }

 /**
  * Get _double
  * minimum: 67.8
  * maximum: 123.4
  * @return _double
  */
  @JsonProperty("double")
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }

  /**
   * Sets the <code>_double</code> property.
   */
 public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   * Sets the <code>_double</code> property.
   */
  public FormatTest _double(Double _double) {
    this._double = _double;
    return this;
  }

 /**
  * Get string
  * @return string
  */
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return string;
  }

  /**
   * Sets the <code>string</code> property.
   */
 public void setString(String string) {
    this.string = string;
  }

  /**
   * Sets the <code>string</code> property.
   */
  public FormatTest string(String string) {
    this.string = string;
    return this;
  }

 /**
  * Get _byte
  * @return _byte
  */
  @JsonProperty("byte")
  @NotNull
  public byte[] getByte() {
    return _byte;
  }

  /**
   * Sets the <code>_byte</code> property.
   */
 public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   * Sets the <code>_byte</code> property.
   */
  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

 /**
  * Get binary
  * @return binary
  */
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }

  /**
   * Sets the <code>binary</code> property.
   */
 public void setBinary(File binary) {
    this.binary = binary;
  }

  /**
   * Sets the <code>binary</code> property.
   */
  public FormatTest binary(File binary) {
    this.binary = binary;
    return this;
  }

 /**
  * Get date
  * @return date
  */
  @JsonProperty("date")
  @NotNull
  public LocalDate getDate() {
    return date;
  }

  /**
   * Sets the <code>date</code> property.
   */
 public void setDate(LocalDate date) {
    this.date = date;
  }

  /**
   * Sets the <code>date</code> property.
   */
  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }

 /**
  * Get dateTime
  * @return dateTime
  */
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  /**
   * Sets the <code>dateTime</code> property.
   */
 public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   * Sets the <code>dateTime</code> property.
   */
  public FormatTest dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
  * Get uuid
  * @return uuid
  */
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }

  /**
   * Sets the <code>uuid</code> property.
   */
 public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  /**
   * Sets the <code>uuid</code> property.
   */
  public FormatTest uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

 /**
  * Get password
  * @return password
  */
  @JsonProperty("password")
  @NotNull
 @Size(min=10,max=64)  public String getPassword() {
    return password;
  }

  /**
   * Sets the <code>password</code> property.
   */
 public void setPassword(String password) {
    this.password = password;
  }

  /**
   * Sets the <code>password</code> property.
   */
  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

 /**
  * Get bigDecimal
  * @return bigDecimal
  */
  @JsonProperty("BigDecimal")
  public BigDecimal getBigDecimal() {
    return bigDecimal;
  }

  /**
   * Sets the <code>bigDecimal</code> property.
   */
 public void setBigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
  }

  /**
   * Sets the <code>bigDecimal</code> property.
   */
  public FormatTest bigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
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
    sb.append("    bigDecimal: ").append(toIndentedString(bigDecimal)).append("\n");
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

