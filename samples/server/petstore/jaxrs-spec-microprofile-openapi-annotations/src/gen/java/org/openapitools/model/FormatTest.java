package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.File;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.UUID;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("format_test")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.8.0-SNAPSHOT")
public class FormatTest  implements Serializable {
  private Integer _integer;
  private Integer int32;
  private Long int64;
  private BigDecimal number;
  private Float _float;
  private Double _double;
  private String _string;
  private byte[] _byte;
  private File binary;
  private LocalDate _date;
  private LocalDateTime _dateTime;
  private UUID _uuid;
  private String password;
  private BigDecimal _bigDecimal;

  protected FormatTest(FormatTestBuilder<?, ?> b) {
    this._integer = b._integer;
    this.int32 = b.int32;
    this.int64 = b.int64;
    this.number = b.number;
    this._float = b._float;
    this._double = b._double;
    this._string = b._string;
    this._byte = b._byte;
    this.binary = b.binary;
    this._date = b._date;
    this._dateTime = b._dateTime;
    this._uuid = b._uuid;
    this.password = b.password;
    this._bigDecimal = b._bigDecimal;
  }

  public FormatTest() {
  }

  /**
   * minimum: 10
   * maximum: 100
   **/
  public FormatTest _integer(Integer _integer) {
    this._integer = _integer;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("integer")
   @Min(10) @Max(100)public Integer getInteger() {
    return _integer;
  }

  @JsonProperty("integer")
  public void setInteger(Integer _integer) {
    this._integer = _integer;
  }

  /**
   * minimum: 20
   * maximum: 200
   **/
  public FormatTest int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("int32")
   @Min(20) @Max(200)public Integer getInt32() {
    return int32;
  }

  @JsonProperty("int32")
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   **/
  public FormatTest int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }

  @JsonProperty("int64")
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

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("number")
  @NotNull @Valid  @DecimalMin("32.1") @DecimalMax("543.2")public BigDecimal getNumber() {
    return number;
  }

  @JsonProperty("number")
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

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("float")
   @DecimalMin("54.3") @DecimalMax("987.6")public Float getFloat() {
    return _float;
  }

  @JsonProperty("float")
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

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("double")
   @DecimalMin("67.8") @DecimalMax("123.4")public Double getDouble() {
    return _double;
  }

  @JsonProperty("double")
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   **/
  public FormatTest _string(String _string) {
    this._string = _string;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("string")
   @Pattern(regexp="/[a-z]/i")public String getString() {
    return _string;
  }

  @JsonProperty("string")
  public void setString(String _string) {
    this._string = _string;
  }

  /**
   **/
  public FormatTest _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("byte")
  @NotNull  @Pattern(regexp="^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$")public byte[] getByte() {
    return _byte;
  }

  @JsonProperty("byte")
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   **/
  public FormatTest binary(File binary) {
    this.binary = binary;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }

  @JsonProperty("binary")
  public void setBinary(File binary) {
    this.binary = binary;
  }

  /**
   **/
  public FormatTest _date(LocalDate _date) {
    this._date = _date;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("date")
  @NotNull public LocalDate getDate() {
    return _date;
  }

  @JsonProperty("date")
  public void setDate(LocalDate _date) {
    this._date = _date;
  }

  /**
   **/
  public FormatTest _dateTime(LocalDateTime _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("dateTime")
  public LocalDateTime getDateTime() {
    return _dateTime;
  }

  @JsonProperty("dateTime")
  public void setDateTime(LocalDateTime _dateTime) {
    this._dateTime = _dateTime;
  }

  /**
   **/
  public FormatTest _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", description = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return _uuid;
  }

  @JsonProperty("uuid")
  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  /**
   **/
  public FormatTest password(String password) {
    this.password = password;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("password")
  @NotNull  @Size(min=10,max=64)public String getPassword() {
    return password;
  }

  @JsonProperty("password")
  public void setPassword(String password) {
    this.password = password;
  }

  /**
   **/
  public FormatTest _bigDecimal(BigDecimal _bigDecimal) {
    this._bigDecimal = _bigDecimal;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("BigDecimal")
  @Valid public BigDecimal getBigDecimal() {
    return _bigDecimal;
  }

  @JsonProperty("BigDecimal")
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


  public static FormatTestBuilder<?, ?> builder() {
    return new FormatTestBuilderImpl();
  }

  private static class FormatTestBuilderImpl extends FormatTestBuilder<FormatTest, FormatTestBuilderImpl> {

    @Override
    protected FormatTestBuilderImpl self() {
      return this;
    }

    @Override
    public FormatTest build() {
      return new FormatTest(this);
    }
  }

  public static abstract class FormatTestBuilder<C extends FormatTest, B extends FormatTestBuilder<C, B>>  {
    private Integer _integer;
    private Integer int32;
    private Long int64;
    private BigDecimal number;
    private Float _float;
    private Double _double;
    private String _string;
    private byte[] _byte;
    private File binary;
    private LocalDate _date;
    private LocalDateTime _dateTime;
    private UUID _uuid;
    private String password;
    private BigDecimal _bigDecimal;
    protected abstract B self();

    public abstract C build();

    public B _integer(Integer _integer) {
      this._integer = _integer;
      return self();
    }
    public B int32(Integer int32) {
      this.int32 = int32;
      return self();
    }
    public B int64(Long int64) {
      this.int64 = int64;
      return self();
    }
    public B number(BigDecimal number) {
      this.number = number;
      return self();
    }
    public B _float(Float _float) {
      this._float = _float;
      return self();
    }
    public B _double(Double _double) {
      this._double = _double;
      return self();
    }
    public B _string(String _string) {
      this._string = _string;
      return self();
    }
    public B _byte(byte[] _byte) {
      this._byte = _byte;
      return self();
    }
    public B binary(File binary) {
      this.binary = binary;
      return self();
    }
    public B _date(LocalDate _date) {
      this._date = _date;
      return self();
    }
    public B _dateTime(LocalDateTime _dateTime) {
      this._dateTime = _dateTime;
      return self();
    }
    public B _uuid(UUID _uuid) {
      this._uuid = _uuid;
      return self();
    }
    public B password(String password) {
      this.password = password;
      return self();
    }
    public B _bigDecimal(BigDecimal _bigDecimal) {
      this._bigDecimal = _bigDecimal;
      return self();
    }
  }
}

