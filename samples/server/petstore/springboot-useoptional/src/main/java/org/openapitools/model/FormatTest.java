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
import org.springframework.lang.Nullable;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class FormatTest {

  private Optional<@Min(10) @Max(100) Integer> integer = Optional.empty();

  private Optional<@Min(20) @Max(200) Integer> int32 = Optional.empty();

  private Optional<Long> int64 = Optional.empty();

  private BigDecimal number;

  private Optional<@DecimalMin("54.3") @DecimalMax("987.6") Float> _float = Optional.empty();

  private Optional<@DecimalMin("67.8") @DecimalMax("123.4") Double> _double = Optional.empty();

  private Optional<@Pattern(regexp = "/[a-z]/i") String> string = Optional.empty();

  private byte[] _byte;

  private Optional<org.springframework.core.io.Resource> binary = Optional.empty();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate date;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> dateTime = Optional.empty();

  private Optional<UUID> uuid = Optional.empty();

  private String password;

  private Optional<BigDecimal> bigDecimal = Optional.empty();

  public FormatTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FormatTest(BigDecimal number, byte[] _byte, LocalDate date, String password) {
    this.number = number;
    this._byte = _byte;
    this.date = date;
    this.password = password;
  }

  public FormatTest integer(Integer integer) {
    this.integer = Optional.ofNullable(integer);
    return this;
  }

  /**
   * Get integer
   * minimum: 10
   * maximum: 100
   * @return integer
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
  public Optional<@Min(10) @Max(100) Integer> getInteger() {
    return integer;
  }

  public void setInteger(Optional<Integer> integer) {
    this.integer = integer;
  }

  public FormatTest int32(Integer int32) {
    this.int32 = Optional.ofNullable(int32);
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
    this.int64 = Optional.ofNullable(int64);
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
    this._float = Optional.ofNullable(_float);
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
    this._double = Optional.ofNullable(_double);
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

  public FormatTest string(String string) {
    this.string = Optional.ofNullable(string);
    return this;
  }

  /**
   * Get string
   * @return string
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("string")
  public Optional<@Pattern(regexp = "/[a-z]/i") String> getString() {
    return string;
  }

  public void setString(Optional<String> string) {
    this.string = string;
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
    this.binary = Optional.ofNullable(binary);
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

  public FormatTest date(LocalDate date) {
    this.date = date;
    return this;
  }

  /**
   * Get date
   * @return date
   */
  @NotNull @Valid 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("date")
  public LocalDate getDate() {
    return date;
  }

  public void setDate(LocalDate date) {
    this.date = date;
  }

  public FormatTest dateTime(OffsetDateTime dateTime) {
    this.dateTime = Optional.ofNullable(dateTime);
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Optional<OffsetDateTime> getDateTime() {
    return dateTime;
  }

  public void setDateTime(Optional<OffsetDateTime> dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest uuid(UUID uuid) {
    this.uuid = Optional.ofNullable(uuid);
    return this;
  }

  /**
   * Get uuid
   * @return uuid
   */
  @Valid 
  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  @JsonProperty("uuid")
  public Optional<UUID> getUuid() {
    return uuid;
  }

  public void setUuid(Optional<UUID> uuid) {
    this.uuid = uuid;
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

  public FormatTest bigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = Optional.ofNullable(bigDecimal);
    return this;
  }

  /**
   * Get bigDecimal
   * @return bigDecimal
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("BigDecimal")
  public Optional<BigDecimal> getBigDecimal() {
    return bigDecimal;
  }

  public void setBigDecimal(Optional<BigDecimal> bigDecimal) {
    this.bigDecimal = bigDecimal;
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
    return Objects.equals(this.integer, formatTest.integer) &&
        Objects.equals(this.int32, formatTest.int32) &&
        Objects.equals(this.int64, formatTest.int64) &&
        Objects.equals(this.number, formatTest.number) &&
        Objects.equals(this._float, formatTest._float) &&
        Objects.equals(this._double, formatTest._double) &&
        Objects.equals(this.string, formatTest.string) &&
        Arrays.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this.date, formatTest.date) &&
        Objects.equals(this.dateTime, formatTest.dateTime) &&
        Objects.equals(this.uuid, formatTest.uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this.bigDecimal, formatTest.bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, Arrays.hashCode(_byte), binary, date, dateTime, uuid, password, bigDecimal);
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
    sb.append("    password: ").append("*").append("\n");
    sb.append("    bigDecimal: ").append(toIndentedString(bigDecimal)).append("\n");
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
      this.instance.setInteger(value.integer);
      this.instance.setInt32(value.int32);
      this.instance.setInt64(value.int64);
      this.instance.setNumber(value.number);
      this.instance.setFloat(value._float);
      this.instance.setDouble(value._double);
      this.instance.setString(value.string);
      this.instance.setByte(value._byte);
      this.instance.setBinary(value.binary);
      this.instance.setDate(value.date);
      this.instance.setDateTime(value.dateTime);
      this.instance.setUuid(value.uuid);
      this.instance.setPassword(value.password);
      this.instance.setBigDecimal(value.bigDecimal);
      return this;
    }

    public FormatTest.Builder integer(Integer integer) {
      this.instance.integer(integer);
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
    
    public FormatTest.Builder string(String string) {
      this.instance.string(string);
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
    
    public FormatTest.Builder date(LocalDate date) {
      this.instance.date(date);
      return this;
    }
    
    public FormatTest.Builder dateTime(OffsetDateTime dateTime) {
      this.instance.dateTime(dateTime);
      return this;
    }
    
    public FormatTest.Builder uuid(UUID uuid) {
      this.instance.uuid(uuid);
      return this;
    }
    
    public FormatTest.Builder password(String password) {
      this.instance.password(password);
      return this;
    }
    
    public FormatTest.Builder bigDecimal(BigDecimal bigDecimal) {
      this.instance.bigDecimal(bigDecimal);
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

