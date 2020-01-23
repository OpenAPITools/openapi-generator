package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.springframework.core.io.Resource;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * TestEndpointParametersBody
 */

public class TestEndpointParametersBody   {
  @JsonProperty("integer")
  private Integer integer;

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
  private String string;

  @JsonProperty("pattern_without_delimiter")
  private String patternWithoutDelimiter;

  @JsonProperty("byte")
  private byte[] _byte;

  @JsonProperty("binary")
  private Resource binary;

  @JsonProperty("date")
  private LocalDate date;

  @JsonProperty("dateTime")
  private OffsetDateTime dateTime;

  @JsonProperty("password")
  private String password;

  @JsonProperty("callback")
  private String callback;

  public TestEndpointParametersBody integer(Integer integer) {
    this.integer = integer;
    return this;
  }

  /**
   * None
   * minimum: 10
   * maximum: 100
   * @return integer
  */
  @ApiModelProperty(value = "None")

@Min(10) @Max(100) 
  public Integer getInteger() {
    return integer;
  }

  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  public TestEndpointParametersBody int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  /**
   * None
   * minimum: 20
   * maximum: 200
   * @return int32
  */
  @ApiModelProperty(value = "None")

@Min(20) @Max(200) 
  public Integer getInt32() {
    return int32;
  }

  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  public TestEndpointParametersBody int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  /**
   * None
   * @return int64
  */
  @ApiModelProperty(value = "None")


  public Long getInt64() {
    return int64;
  }

  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  public TestEndpointParametersBody number(BigDecimal number) {
    this.number = number;
    return this;
  }

  /**
   * None
   * minimum: 32.1
   * maximum: 543.2
   * @return number
  */
  @ApiModelProperty(required = true, value = "None")
  @NotNull

  @Valid
@DecimalMin("32.1") @DecimalMax("543.2") 
  public BigDecimal getNumber() {
    return number;
  }

  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  public TestEndpointParametersBody _float(Float _float) {
    this._float = _float;
    return this;
  }

  /**
   * None
   * maximum: 987.6
   * @return _float
  */
  @ApiModelProperty(value = "None")

 @DecimalMax("987.6") 
  public Float getFloat() {
    return _float;
  }

  public void setFloat(Float _float) {
    this._float = _float;
  }

  public TestEndpointParametersBody _double(Double _double) {
    this._double = _double;
    return this;
  }

  /**
   * None
   * minimum: 67.8
   * maximum: 123.4
   * @return _double
  */
  @ApiModelProperty(required = true, value = "None")
  @NotNull

@DecimalMin("67.8") @DecimalMax("123.4") 
  public Double getDouble() {
    return _double;
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }

  public TestEndpointParametersBody string(String string) {
    this.string = string;
    return this;
  }

  /**
   * None
   * @return string
  */
  @ApiModelProperty(value = "None")

@Pattern(regexp="/[a-z]/i") 
  public String getString() {
    return string;
  }

  public void setString(String string) {
    this.string = string;
  }

  public TestEndpointParametersBody patternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
    return this;
  }

  /**
   * None
   * @return patternWithoutDelimiter
  */
  @ApiModelProperty(required = true, value = "None")
  @NotNull

@Pattern(regexp="^[A-Z].*") 
  public String getPatternWithoutDelimiter() {
    return patternWithoutDelimiter;
  }

  public void setPatternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
  }

  public TestEndpointParametersBody _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  /**
   * None
   * @return _byte
  */
  @ApiModelProperty(required = true, value = "None")
  @NotNull


  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public TestEndpointParametersBody binary(Resource binary) {
    this.binary = binary;
    return this;
  }

  /**
   * None
   * @return binary
  */
  @ApiModelProperty(value = "None")

  @Valid

  public Resource getBinary() {
    return binary;
  }

  public void setBinary(Resource binary) {
    this.binary = binary;
  }

  public TestEndpointParametersBody date(LocalDate date) {
    this.date = date;
    return this;
  }

  /**
   * None
   * @return date
  */
  @ApiModelProperty(value = "None")

  @Valid

  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public TestEndpointParametersBody dateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  /**
   * None
   * @return dateTime
  */
  @ApiModelProperty(value = "None")

  @Valid

  public OffsetDateTime getDateTime() {
    return dateTime;
  }

  public void setDateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
  }

  public TestEndpointParametersBody password(String password) {
    this.password = password;
    return this;
  }

  /**
   * None
   * @return password
  */
  @ApiModelProperty(value = "None")

@Size(min=10,max=64) 
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public TestEndpointParametersBody callback(String callback) {
    this.callback = callback;
    return this;
  }

  /**
   * None
   * @return callback
  */
  @ApiModelProperty(value = "None")


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
    TestEndpointParametersBody testEndpointParametersBody = (TestEndpointParametersBody) o;
    return Objects.equals(this.integer, testEndpointParametersBody.integer) &&
        Objects.equals(this.int32, testEndpointParametersBody.int32) &&
        Objects.equals(this.int64, testEndpointParametersBody.int64) &&
        Objects.equals(this.number, testEndpointParametersBody.number) &&
        Objects.equals(this._float, testEndpointParametersBody._float) &&
        Objects.equals(this._double, testEndpointParametersBody._double) &&
        Objects.equals(this.string, testEndpointParametersBody.string) &&
        Objects.equals(this.patternWithoutDelimiter, testEndpointParametersBody.patternWithoutDelimiter) &&
        Objects.equals(this._byte, testEndpointParametersBody._byte) &&
        Objects.equals(this.binary, testEndpointParametersBody.binary) &&
        Objects.equals(this.date, testEndpointParametersBody.date) &&
        Objects.equals(this.dateTime, testEndpointParametersBody.dateTime) &&
        Objects.equals(this.password, testEndpointParametersBody.password) &&
        Objects.equals(this.callback, testEndpointParametersBody.callback);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, patternWithoutDelimiter, _byte, binary, date, dateTime, password, callback);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestEndpointParametersBody {\n");
    
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

