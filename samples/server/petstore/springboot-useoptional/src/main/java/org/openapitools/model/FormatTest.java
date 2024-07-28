package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.UUID;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * FormatTest
 */

@JsonTypeName("format_test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 8.0.0-SNAPSHOT")
public class FormatTest {

  private Optional<@Min(10) @Max(100) Integer> _integer = Optional.empty();

  private Optional<@Min(20) @Max(200) Integer> int32 = Optional.empty();

  private Optional<Long> int64 = Optional.empty();

  private BigDecimal number;

  private Optional<@DecimalMin("54.3") @DecimalMax("987.6") Float> _float = Optional.empty();

  private Optional<@DecimalMin("67.8") @DecimalMax("123.4") Double> _double = Optional.empty();

  private Optional<@Pattern(regexp = "/[a-z]/i") String> _string = Optional.empty();

  private byte[] _byte;

  private Optional<org.springframework.core.io.Resource> binary = Optional.empty();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate _date;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> _dateTime = Optional.empty();

  private Optional<UUID> _uuid = Optional.empty();

  private String password;

  private Optional<BigDecimal> _bigDecimal = Optional.empty();

  public FormatTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FormatTest(BigDecimal number, byte[] _byte, LocalDate _date, String password) {
    this.number = number;
    this._byte = _byte;
    this._date = _date;
    this.password = password;
  }

  public FormatTest _integer(Integer _integer) {
    this._integer = Optional.of(_integer);
    return this;
  }

  /**
   * Get _integer
   * minimum: 10
   * maximum: 100
   * @return _integer
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
  public Optional<@Min(10) @Max(100) Integer> getInteger() {
    return _integer;
  }

  public void setInteger(Optional<Integer> _integer) {
    this._integer = _integer;
  }

  public FormatTest int32(Integer int32) {
    this.int32 = Optional.of(int32);
    return this;
  }

  /**
   * Get int32
   * minimum: 20
   * maximum: 200
   * @return int32
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("int32")
  public Optional<@Min(20) @Max(200) Integer> getInt32() {
    return int32;
  }

  public void setInt32(Optional<Integer> int32) {
    this.int32 = int32;
  }

  public FormatTest int64(Long int64) {
    this.int64 = Optional.of(int64);
    return this;
  }

  /**
   * Get int64
   * @return int64
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("int64")
  public Optional<Long> getInt64() {
    return int64;
  }

  public void setInt64(Optional<Long> int64) {
    this.int64 = int64;
  }

  public FormatTest number(BigDecimal number) {
    this.number = number;
    return this;
  }

  /**
   * Get number
   * minimum: 32.1
   * maximum: 543.2
   * @return number
   */
  @NotNull @Valid @DecimalMin("32.1") @DecimalMax("543.2") 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number")
  public BigDecimal getNumber() {
    return number;
  }

  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  public FormatTest _float(Float _float) {
    this._float = Optional.of(_float);
    return this;
  }

  /**
   * Get _float
   * minimum: 54.3
   * maximum: 987.6
   * @return _float
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("float")
  public Optional<@DecimalMin("54.3") @DecimalMax("987.6") Float> getFloat() {
    return _float;
  }

  public void setFloat(Optional<Float> _float) {
    this._float = _float;
  }

  public FormatTest _double(Double _double) {
    this._double = Optional.of(_double);
    return this;
  }

  /**
   * Get _double
   * minimum: 67.8
   * maximum: 123.4
   * @return _double
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("double")
  public Optional<@DecimalMin("67.8") @DecimalMax("123.4") Double> getDouble() {
    return _double;
  }

  public void setDouble(Optional<Double> _double) {
    this._double = _double;
  }

  public FormatTest _string(String _string) {
    this._string = Optional.of(_string);
    return this;
  }

  /**
   * Get _string
   * @return _string
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("string")
  public Optional<@Pattern(regexp = "/[a-z]/i") String> getString() {
    return _string;
  }

  public void setString(Optional<String> _string) {
    this._string = _string;
  }

  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  /**
   * Get _byte
   * @return _byte
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("byte")
  public byte[] getByte() {
    return _byte;
  }

  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  public FormatTest binary(org.springframework.core.io.Resource binary) {
    this.binary = Optional.of(binary);
    return this;
  }

  /**
   * Get binary
   * @return binary
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("binary")
  public Optional<org.springframework.core.io.Resource> getBinary() {
    return binary;
  }

  public void setBinary(Optional<org.springframework.core.io.Resource> binary) {
    this.binary = binary;
  }

  public FormatTest _date(LocalDate _date) {
    this._date = _date;
    return this;
  }

  /**
   * Get _date
   * @return _date
   */
  @NotNull @Valid 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("date")
  public LocalDate getDate() {
    return _date;
  }

  public void setDate(LocalDate _date) {
    this._date = _date;
  }

  public FormatTest _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = Optional.of(_dateTime);
    return this;
  }

  /**
   * Get _dateTime
   * @return _dateTime
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Optional<OffsetDateTime> getDateTime() {
    return _dateTime;
  }

  public void setDateTime(Optional<OffsetDateTime> _dateTime) {
    this._dateTime = _dateTime;
  }

  public FormatTest _uuid(UUID _uuid) {
    this._uuid = Optional.of(_uuid);
    return this;
  }

  /**
   * Get _uuid
   * @return _uuid
   */
  @Valid 
  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  @JsonProperty("uuid")
  public Optional<UUID> getUuid() {
    return _uuid;
  }

  public void setUuid(Optional<UUID> _uuid) {
    this._uuid = _uuid;
  }

  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

  /**
   * Get password
   * @return password
   */
  @NotNull @Size(min = 10, max = 64) 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public FormatTest _bigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = Optional.of(_bigDecimal);
    return this;
  }

  /**
   * Get _bigDecimal
   * @return _bigDecimal
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("BigDecimal")
  public Optional<BigDecimal> getBigDecimal() {
    return _bigDecimal;
  }

  public void setBigDecimal(Optional<BigDecimal> _bigDecimal) {
    this._bigDecimal = _bigDecimal;
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
    return Objects.equals(this._integer, formatTest._integer) &&
        Objects.equals(this.int32, formatTest.int32) &&
        Objects.equals(this.int64, formatTest.int64) &&
        Objects.equals(this.number, formatTest.number) &&
        Objects.equals(this._float, formatTest._float) &&
        Objects.equals(this._double, formatTest._double) &&
        Objects.equals(this._string, formatTest._string) &&
        Arrays.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this._date, formatTest._date) &&
        Objects.equals(this._dateTime, formatTest._dateTime) &&
        Objects.equals(this._uuid, formatTest._uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this._bigDecimal, formatTest._bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_integer, int32, int64, number, _float, _double, _string, Arrays.hashCode(_byte), binary, _date, _dateTime, _uuid, password, _bigDecimal);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FormatTest {\n");
    sb.append("    _integer: ").append(toIndentedString(_integer)).append("\n");
    sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
    sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
    sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
    sb.append("    _string: ").append(toIndentedString(_string)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    _date: ").append(toIndentedString(_date)).append("\n");
    sb.append("    _dateTime: ").append(toIndentedString(_dateTime)).append("\n");
    sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
    sb.append("    password: ").append("*").append("\n");
    sb.append("    _bigDecimal: ").append(toIndentedString(_bigDecimal)).append("\n");
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
  
  public static class Builder {

    private FormatTest instance;

    public Builder() {
      this(new FormatTest());
    }

    protected Builder(FormatTest instance) {
      this.instance = instance;
    }

    protected Builder copyOf(FormatTest value) { 
      this.instance.setInteger(value._integer);
      this.instance.setInt32(value.int32);
      this.instance.setInt64(value.int64);
      this.instance.setNumber(value.number);
      this.instance.setFloat(value._float);
      this.instance.setDouble(value._double);
      this.instance.setString(value._string);
      this.instance.setByte(value._byte);
      this.instance.setBinary(value.binary);
      this.instance.setDate(value._date);
      this.instance.setDateTime(value._dateTime);
      this.instance.setUuid(value._uuid);
      this.instance.setPassword(value.password);
      this.instance.setBigDecimal(value._bigDecimal);
      return this;
    }

    public FormatTest.Builder _integer(Integer _integer) {
      this.instance._integer(_integer);
      return this;
    }
    
    public FormatTest.Builder int32(Integer int32) {
      this.instance.int32(int32);
      return this;
    }
    
    public FormatTest.Builder int64(Long int64) {
      this.instance.int64(int64);
      return this;
    }
    
    public FormatTest.Builder number(BigDecimal number) {
      this.instance.number(number);
      return this;
    }
    
    public FormatTest.Builder _float(Float _float) {
      this.instance._float(_float);
      return this;
    }
    
    public FormatTest.Builder _double(Double _double) {
      this.instance._double(_double);
      return this;
    }
    
    public FormatTest.Builder _string(String _string) {
      this.instance._string(_string);
      return this;
    }
    
    public FormatTest.Builder _byte(byte[] _byte) {
      this.instance._byte(_byte);
      return this;
    }
    
    public FormatTest.Builder binary(org.springframework.core.io.Resource binary) {
      this.instance.binary(binary);
      return this;
    }
    
    public FormatTest.Builder _date(LocalDate _date) {
      this.instance._date(_date);
      return this;
    }
    
    public FormatTest.Builder _dateTime(OffsetDateTime _dateTime) {
      this.instance._dateTime(_dateTime);
      return this;
    }
    
    public FormatTest.Builder _uuid(UUID _uuid) {
      this.instance._uuid(_uuid);
      return this;
    }
    
    public FormatTest.Builder password(String password) {
      this.instance.password(password);
      return this;
    }
    
    public FormatTest.Builder _bigDecimal(BigDecimal _bigDecimal) {
      this.instance._bigDecimal(_bigDecimal);
      return this;
    }
    
    /**
    * returns a built FormatTest instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public FormatTest build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static FormatTest.Builder builder() {
    return new FormatTest.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public FormatTest.Builder toBuilder() {
    FormatTest.Builder builder = new FormatTest.Builder();
    return builder.copyOf(this);
  }

}

