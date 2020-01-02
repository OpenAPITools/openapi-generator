package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.File;
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
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class FormatTest  implements Serializable {
  
  private @Valid Integer integer;
  private @Valid Integer int32;
  private @Valid Long int64;
  private @Valid BigDecimal number;
  private @Valid Float _float;
  private @Valid Double _double;
  private @Valid String string;
  private @Valid byte[] _byte;
  private @Valid File binary;
  private @Valid LocalDate date;
  private @Valid Date dateTime;
  private @Valid UUID uuid;
  private @Valid String password;
  private @Valid BigDecimal bigDecimal;

  public FormatTest(Integer integer, Integer int32, Long int64, BigDecimal number, Float _float, Double _double, String string, byte[] _byte, File binary, LocalDate date, Date dateTime, UUID uuid, String password, BigDecimal bigDecimal) {
    this.integer = integer;
    this.int32 = int32;
    this.int64 = int64;
    this.number = number;
    this._float = _float;
    this._double = _double;
    this.string = string;
    this._byte = _byte;
    this.binary = binary;
    this.date = date;
    this.dateTime = dateTime;
    this.uuid = uuid;
    this.password = password;
    this.bigDecimal = bigDecimal;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("integer")
 @Min(10) @Max(100)  public Integer getInteger() {
    return integer;
  }

  public void setInteger(Integer integer) {
    this.integer = integer;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("int32")
 @Min(20) @Max(200)  public Integer getInt32() {
    return int32;
  }

  public void setInt32(Integer int32) {
    this.int32 = int32;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }

  public void setInt64(Long int64) {
    this.int64 = int64;
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
  @ApiModelProperty(value = "")
  @JsonProperty("float")
 @DecimalMin("54.3") @DecimalMax("987.6")  public Float getFloat() {
    return _float;
  }

  public void setFloat(Float _float) {
    this._float = _float;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("double")
 @DecimalMin("67.8") @DecimalMax("123.4")  public Double getDouble() {
    return _double;
  }

  public void setDouble(Double _double) {
    this._double = _double;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("string")
 @Pattern(regexp="/[a-z]/i")  public String getString() {
    return string;
  }

  public void setString(String string) {
    this.string = string;
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
  @ApiModelProperty(value = "")
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }

  public void setBinary(File binary) {
    this.binary = binary;
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
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }
  @ApiModelProperty(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", value = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }

  public void setUuid(UUID uuid) {
    this.uuid = uuid;
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
  @ApiModelProperty(value = "")
  @JsonProperty("BigDecimal")
  public BigDecimal getBigDecimal() {
    return bigDecimal;
  }

  public void setBigDecimal(BigDecimal bigDecimal) {
    this.bigDecimal = bigDecimal;
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
    return Objects.equals(this.integer, formatTest.integer) &&
        Objects.equals(this.int32, formatTest.int32) &&
        Objects.equals(this.int64, formatTest.int64) &&
        Objects.equals(this.number, formatTest.number) &&
        Objects.equals(this._float, formatTest._float) &&
        Objects.equals(this._double, formatTest._double) &&
        Objects.equals(this.string, formatTest.string) &&
        Objects.equals(this._byte, formatTest._byte) &&
        Objects.equals(this.binary, formatTest.binary) &&
        Objects.equals(this.date, formatTest.date) &&
        Objects.equals(this.dateTime, formatTest.dateTime) &&
        Objects.equals(this.uuid, formatTest.uuid) &&
        Objects.equals(this.password, formatTest.password) &&
        Objects.equals(this.bigDecimal, formatTest.bigDecimal);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, _byte, binary, date, dateTime, uuid, password, bigDecimal);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private Integer integer;
    private Integer int32;
    private Long int64;
    private BigDecimal number;
    private Float _float;
    private Double _double;
    private String string;
    private byte[] _byte;
    private File binary;
    private LocalDate date;
    private Date dateTime;
    private UUID uuid;
    private String password;
    private BigDecimal bigDecimal;

    /**
      * minimum: 10
      * maximum: 100
      **/
    public Builder integer(Integer integer) {
      this.integer = integer;
      return this;
    }
    /**
      * minimum: 20
      * maximum: 200
      **/
    public Builder int32(Integer int32) {
      this.int32 = int32;
      return this;
    }
    /**
      **/
    public Builder int64(Long int64) {
      this.int64 = int64;
      return this;
    }
    /**
      * minimum: 32.1
      * maximum: 543.2
      **/
    public Builder number(BigDecimal number) {
      this.number = number;
      return this;
    }
    /**
      * minimum: 54.3
      * maximum: 987.6
      **/
    public Builder _float(Float _float) {
      this._float = _float;
      return this;
    }
    /**
      * minimum: 67.8
      * maximum: 123.4
      **/
    public Builder _double(Double _double) {
      this._double = _double;
      return this;
    }
    /**
      **/
    public Builder string(String string) {
      this.string = string;
      return this;
    }
    /**
      **/
    public Builder _byte(byte[] _byte) {
      this._byte = _byte;
      return this;
    }
    /**
      **/
    public Builder binary(File binary) {
      this.binary = binary;
      return this;
    }
    /**
      **/
    public Builder date(LocalDate date) {
      this.date = date;
      return this;
    }
    /**
      **/
    public Builder dateTime(Date dateTime) {
      this.dateTime = dateTime;
      return this;
    }
    /**
      **/
    public Builder uuid(UUID uuid) {
      this.uuid = uuid;
      return this;
    }
    /**
      **/
    public Builder password(String password) {
      this.password = password;
      return this;
    }
    /**
      **/
    public Builder bigDecimal(BigDecimal bigDecimal) {
      this.bigDecimal = bigDecimal;
      return this;
    }

    public FormatTest build() {
      return new FormatTest(integer, int32, int64, number, _float, _double, string, _byte, binary, date, dateTime, uuid, password, bigDecimal);
    }
  }
}

