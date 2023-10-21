package apimodels;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.UUID;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * FormatTest
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class FormatTest   {
  @JsonProperty("integer")
  @Min(10)
@Max(100)

  private Integer _integer;

  @JsonProperty("int32")
  @Min(20)
@Max(200)

  private Integer int32;

  @JsonProperty("int64")
  
  private Long int64;

  @JsonProperty("number")
  @NotNull
@DecimalMin("32.1")
@DecimalMax("543.2")
@Valid

  private BigDecimal number;

  @JsonProperty("float")
  @DecimalMin("54.3")
@DecimalMax("987.6")

  private Float _float;

  @JsonProperty("double")
  @DecimalMin("67.8")
@DecimalMax("123.4")

  private Double _double;

  @JsonProperty("string")
  @Pattern(regexp="/[a-z]/i")

  private String _string;

  @JsonProperty("byte")
  @NotNull
@Pattern(regexp="^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$")

  private byte[] _byte;

  @JsonProperty("binary")
  @Valid

  private InputStream binary;

  @JsonProperty("date")
  @NotNull
@Valid

  private LocalDate _date;

  @JsonProperty("dateTime")
  @Valid

  private OffsetDateTime _dateTime;

  @JsonProperty("uuid")
  @Valid

  private UUID _uuid;

  @JsonProperty("password")
  @NotNull
@Size(min=10,max=64)

  private String password;

  @JsonProperty("BigDecimal")
  @Valid

  private BigDecimal _bigDecimal;

  public FormatTest _integer(Integer _integer) {
    this._integer = _integer;
    return this;
  }

   /**
   * Get _integer
   * minimum: 10
   * maximum: 100
   * @return _integer
  **/
  public Integer getInteger() {
    return _integer;
  }

  public void setInteger(Integer _integer) {
    this._integer = _integer;
  }

  public FormatTest int32(Integer int32) {
    this.int32 = int32;
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

  public FormatTest int64(Long int64) {
    this.int64 = int64;
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

  public FormatTest number(BigDecimal number) {
    this.number = number;
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

  public FormatTest _float(Float _float) {
    this._float = _float;
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

  public FormatTest _double(Double _double) {
    this._double = _double;
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

  public FormatTest _string(String _string) {
    this._string = _string;
    return this;
  }

   /**
   * Get _string
   * @return _string
  **/
  public String getString() {
    return _string;
  }

  public void setString(String _string) {
    this._string = _string;
  }

  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
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

  public FormatTest binary(InputStream binary) {
    this.binary = binary;
    return this;
  }

   /**
   * Get binary
   * @return binary
  **/
  public InputStream getBinary() {
    return binary;
  }

  public void setBinary(InputStream binary) {
    this.binary = binary;
  }

  public FormatTest _date(LocalDate _date) {
    this._date = _date;
    return this;
  }

   /**
   * Get _date
   * @return _date
  **/
  public LocalDate getDate() {
    return _date;
  }

  public void setDate(LocalDate _date) {
    this._date = _date;
  }

  public FormatTest _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

   /**
   * Get _dateTime
   * @return _dateTime
  **/
  public OffsetDateTime getDateTime() {
    return _dateTime;
  }

  public void setDateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
  }

  public FormatTest _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

   /**
   * Get _uuid
   * @return _uuid
  **/
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  public FormatTest password(String password) {
    this.password = password;
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

  public FormatTest _bigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
    return this;
  }

   /**
   * Get _bigDecimal
   * @return _bigDecimal
  **/
  public BigDecimal getBigDecimal() {
    return _bigDecimal;
  }

  public void setBigDecimal(BigDecimal _bigDecimal) {
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
    return Objects.equals(_integer, formatTest._integer) &&
        Objects.equals(int32, formatTest.int32) &&
        Objects.equals(int64, formatTest.int64) &&
        Objects.equals(number, formatTest.number) &&
        Objects.equals(_float, formatTest._float) &&
        Objects.equals(_double, formatTest._double) &&
        Objects.equals(_string, formatTest._string) &&
        Objects.equals(_byte, formatTest._byte) &&
        Objects.equals(binary, formatTest.binary) &&
        Objects.equals(_date, formatTest._date) &&
        Objects.equals(_dateTime, formatTest._dateTime) &&
        Objects.equals(_uuid, formatTest._uuid) &&
        Objects.equals(password, formatTest.password) &&
        Objects.equals(_bigDecimal, formatTest._bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_integer, int32, int64, number, _float, _double, _string, _byte, binary, _date, _dateTime, _uuid, password, _bigDecimal);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
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
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
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
}

