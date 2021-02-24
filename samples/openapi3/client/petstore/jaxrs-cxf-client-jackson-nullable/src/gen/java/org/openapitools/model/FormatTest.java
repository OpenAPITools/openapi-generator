package org.openapitools.model;

import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
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
import com.fasterxml.jackson.annotation.JsonProperty;

public class FormatTest  {
  
  @ApiModelProperty(value = "")
  private Integer integer;

  @ApiModelProperty(value = "")
  private Integer int32;

  @ApiModelProperty(value = "")
  private Long int64;

  @ApiModelProperty(required = true, value = "")
  private BigDecimal number;

  @ApiModelProperty(value = "")
  private Float _float;

  @ApiModelProperty(value = "")
  private Double _double;

  @ApiModelProperty(value = "")
  private BigDecimal decimal;

  @ApiModelProperty(value = "")
  private String string;

  @ApiModelProperty(required = true, value = "")
  private byte[] _byte;

  @ApiModelProperty(value = "")
  private File binary;

  @ApiModelProperty(required = true, value = "")
  private LocalDate date;

  @ApiModelProperty(value = "")
  private Date dateTime;

  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  private UUID uuid;

  @ApiModelProperty(required = true, value = "")
  private String password;

  @ApiModelProperty(value = "A string that is a 10 digit number. Can have leading zeros.")
 /**
   * A string that is a 10 digit number. Can have leading zeros.
  **/
  private String patternWithDigits;

  @ApiModelProperty(value = "A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.")
 /**
   * A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  **/
  private String patternWithDigitsAndDelimiter;
 /**
   * Get integer
   * minimum: 10
   * maximum: 100
   * @return integer
  **/
  @JsonProperty("integer")
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
  @JsonProperty("int32")
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
  @JsonProperty("float")
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
  @JsonProperty("double")
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
   * Get decimal
   * @return decimal
  **/
  @JsonProperty("decimal")
  public BigDecimal getDecimal() {
    return decimal;
  }

  public void setDecimal(BigDecimal decimal) {
    this.decimal = decimal;
  }

  public FormatTest decimal(BigDecimal decimal) {
    this.decimal = decimal;
    return this;
  }

 /**
   * Get string
   * @return string
  **/
  @JsonProperty("string")
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
  @JsonProperty("byte")
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
   * Get date
   * @return date
  **/
  @JsonProperty("date")
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
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
   * Get uuid
   * @return uuid
  **/
  @JsonProperty("uuid")
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
  @JsonProperty("password")
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

 /**
   * A string that is a 10 digit number. Can have leading zeros.
   * @return patternWithDigits
  **/
  @JsonProperty("pattern_with_digits")
  public String getPatternWithDigits() {
    return patternWithDigits;
  }

  public void setPatternWithDigits(String patternWithDigits) {
    this.patternWithDigits = patternWithDigits;
  }

  public FormatTest patternWithDigits(String patternWithDigits) {
    this.patternWithDigits = patternWithDigits;
    return this;
  }

 /**
   * A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.
   * @return patternWithDigitsAndDelimiter
  **/
  @JsonProperty("pattern_with_digits_and_delimiter")
  public String getPatternWithDigitsAndDelimiter() {
    return patternWithDigitsAndDelimiter;
  }

  public void setPatternWithDigitsAndDelimiter(String patternWithDigitsAndDelimiter) {
    this.patternWithDigitsAndDelimiter = patternWithDigitsAndDelimiter;
  }

  public FormatTest patternWithDigitsAndDelimiter(String patternWithDigitsAndDelimiter) {
    this.patternWithDigitsAndDelimiter = patternWithDigitsAndDelimiter;
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
    sb.append("    decimal: ").append(toIndentedString(decimal)).append("\n");
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    date: ").append(toIndentedString(date)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    patternWithDigits: ").append(toIndentedString(patternWithDigits)).append("\n");
    sb.append("    patternWithDigitsAndDelimiter: ").append(toIndentedString(patternWithDigitsAndDelimiter)).append("\n");
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

