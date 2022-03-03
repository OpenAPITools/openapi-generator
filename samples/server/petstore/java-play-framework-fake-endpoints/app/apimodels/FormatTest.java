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

  private Integer integer;

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

  private String string;

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

  private LocalDate date;

  @JsonProperty("dateTime")
  @Valid

  private OffsetDateTime dateTime;

  @JsonProperty("uuid")
  @Valid

  private UUID uuid;

  @JsonProperty("password")
  @NotNull
@Size(min=10,max=64)

  private String password;

  @JsonProperty("BigDecimal")
  @Valid

  private BigDecimal bigDecimal;

  public FormatTest integer(Integer integer) {
    this.integer = integer;
    return this;
  }

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

  public FormatTest string(String string) {
    this.string = string;
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

  public FormatTest date(LocalDate date) {
    this.date = date;
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

  public FormatTest dateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }

   /**
   * Get dateTime
   * @return dateTime
  **/
  public OffsetDateTime getDateTime() {
    return dateTime;
  }

  public void setDateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
  }

  public FormatTest uuid(UUID uuid) {
    this.uuid = uuid;
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

  public FormatTest bigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
    return this;
  }

   /**
   * Get bigDecimal
   * @return bigDecimal
  **/
  public BigDecimal getBigDecimal() {
    return bigDecimal;
  }

  public void setBigDecimal(BigDecimal bigDecimal) {
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
        Objects.equals(password, formatTest.password) &&
        Objects.equals(bigDecimal, formatTest.bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, _byte, binary, date, dateTime, uuid, password, bigDecimal);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

