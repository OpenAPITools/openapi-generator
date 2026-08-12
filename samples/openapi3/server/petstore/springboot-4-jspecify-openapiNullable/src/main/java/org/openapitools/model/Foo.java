package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.format.annotation.DateTimeFormat;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import tools.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import tools.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import tools.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * Foo
 */

@JacksonXmlRootElement(localName = "Foo")
@XmlRootElement(name = "Foo")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.25.0-SNAPSHOT")
public class Foo {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private java.time.@Nullable Instant dt;

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private JsonNullable<java.time.Instant> nullableDt = JsonNullable.<java.time.Instant>undefined();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private org.springframework.core.io.@Nullable Resource binary;

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  private JsonNullable<org.springframework.core.io.Resource> nullableBinary = JsonNullable.<org.springframework.core.io.Resource>undefined();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable List<java.time.Instant> listOfDt = new ArrayList<>();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable List<java.time.Instant> listMinIntems = new ArrayList<>();

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  private JsonNullable<List<java.time.Instant>> nullableListMinIntems = JsonNullable.<List<java.time.Instant>>undefined();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private java.time.Instant requiredDt;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private java.math.@Nullable BigDecimal number;

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  private JsonNullable<java.math.BigDecimal> nullableNumber = JsonNullable.<java.math.BigDecimal>undefined();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String color = "red";

  private String requiredColor = "red";

  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  private JsonNullable<String> nullableColor = JsonNullable.<String>undefined();

  public Foo() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Foo(java.time.Instant requiredDt, String requiredColor) {
    this.requiredDt = requiredDt;
    this.requiredColor = requiredColor;
  }

  /**
   * Constructor with all args parameters
   */
  public Foo(java.time.@Nullable Instant dt, java.time.@Nullable Instant nullableDt, org.springframework.core.io.@Nullable Resource binary, org.springframework.core.io.@Nullable Resource nullableBinary, @Nullable List<java.time.Instant> listOfDt, @Nullable List<java.time.Instant> listMinIntems, @Nullable List<java.time.Instant> nullableListMinIntems, java.time.Instant requiredDt, java.math.@Nullable BigDecimal number, java.math.@Nullable BigDecimal nullableNumber, @Nullable String color, String requiredColor, @Nullable String nullableColor) {
      this.dt = dt;
      this.nullableDt = JsonNullable.of(nullableDt);
      this.binary = binary;
      this.nullableBinary = JsonNullable.of(nullableBinary);
      this.listOfDt = listOfDt;
      this.listMinIntems = listMinIntems;
      this.nullableListMinIntems = JsonNullable.of(nullableListMinIntems);
      this.requiredDt = requiredDt;
      this.number = number;
      this.nullableNumber = JsonNullable.of(nullableNumber);
      this.color = color;
      this.requiredColor = requiredColor;
      this.nullableColor = JsonNullable.of(nullableColor);
  }

  public Foo dt(java.time.@Nullable Instant dt) {
    this.dt = dt;
    return this;
  }

  /**
   * Get dt
   * @return dt
   */
  @Valid 
  @Schema(name = "dt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dt")
  @JacksonXmlProperty(localName = "dt")
  @XmlElement(name = "dt")
  public java.time.@Nullable Instant getDt() {
    return dt;
  }

  @JsonProperty("dt")
  @JacksonXmlProperty(localName = "dt")
  public void setDt(java.time.@Nullable Instant dt) {
    this.dt = dt;
  }

  public Foo nullableDt(java.time.@Nullable Instant nullableDt) {
    this.nullableDt = JsonNullable.of(nullableDt);
    return this;
  }

  /**
   * Get nullableDt
   * @return nullableDt
   */
  @Valid 
  @Schema(name = "nullableDt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullableDt")
  @JacksonXmlProperty(localName = "nullableDt")
  @XmlElement(name = "nullableDt")
  public JsonNullable<java.time.Instant> getNullableDt() {
    return nullableDt;
  }

  public void setNullableDt(JsonNullable<java.time.Instant> nullableDt) {
    this.nullableDt = nullableDt;
  }

  public Foo binary(org.springframework.core.io.@Nullable Resource binary) {
    this.binary = binary;
    return this;
  }

  /**
   * Get binary
   * @return binary
   */
  @Valid 
  @Schema(name = "binary", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("binary")
  @JacksonXmlProperty(localName = "binary")
  @XmlElement(name = "binary")
  public org.springframework.core.io.@Nullable Resource getBinary() {
    return binary;
  }

  @JsonProperty("binary")
  @JacksonXmlProperty(localName = "binary")
  public void setBinary(org.springframework.core.io.@Nullable Resource binary) {
    this.binary = binary;
  }

  public Foo nullableBinary(org.springframework.core.io.@Nullable Resource nullableBinary) {
    this.nullableBinary = JsonNullable.of(nullableBinary);
    return this;
  }

  /**
   * Get nullableBinary
   * @return nullableBinary
   */
  @Valid 
  @Schema(name = "nullableBinary", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullableBinary")
  @JacksonXmlProperty(localName = "nullableBinary")
  @XmlElement(name = "nullableBinary")
  public JsonNullable<org.springframework.core.io.Resource> getNullableBinary() {
    return nullableBinary;
  }

  public void setNullableBinary(JsonNullable<org.springframework.core.io.Resource> nullableBinary) {
    this.nullableBinary = nullableBinary;
  }

  public Foo listOfDt(@Nullable List<java.time.Instant> listOfDt) {
    this.listOfDt = listOfDt;
    return this;
  }

  public Foo addListOfDtItem(java.time.Instant listOfDtItem) {
    if (this.listOfDt == null) {
      this.listOfDt = new ArrayList<>();
    }
    this.listOfDt.add(listOfDtItem);
    return this;
  }

  /**
   * Get listOfDt
   * @return listOfDt
   */
  @Valid 
  @Schema(name = "listOfDt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("listOfDt")
  @JacksonXmlProperty(localName = "listOfDt")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "listOfDt")
  public @Nullable List<java.time.Instant> getListOfDt() {
    return listOfDt;
  }

  @JsonProperty("listOfDt")
  @JacksonXmlProperty(localName = "listOfDt")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListOfDt(@Nullable List<java.time.Instant> listOfDt) {
    this.listOfDt = listOfDt;
  }

  public Foo listMinIntems(@Nullable List<java.time.Instant> listMinIntems) {
    this.listMinIntems = listMinIntems;
    return this;
  }

  public Foo addListMinIntemsItem(java.time.Instant listMinIntemsItem) {
    if (this.listMinIntems == null) {
      this.listMinIntems = new ArrayList<>();
    }
    this.listMinIntems.add(listMinIntemsItem);
    return this;
  }

  /**
   * Get listMinIntems
   * @return listMinIntems
   */
  @Valid @Size(min = 1) 
  @Schema(name = "listMinIntems", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("listMinIntems")
  @JacksonXmlProperty(localName = "listMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "listMinIntems")
  public @Nullable List<java.time.Instant> getListMinIntems() {
    return listMinIntems;
  }

  @JsonProperty("listMinIntems")
  @JacksonXmlProperty(localName = "listMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListMinIntems(@Nullable List<java.time.Instant> listMinIntems) {
    this.listMinIntems = listMinIntems;
  }

  public Foo nullableListMinIntems(@Nullable List<java.time.Instant> nullableListMinIntems) {
    this.nullableListMinIntems = JsonNullable.of(nullableListMinIntems);
    return this;
  }

  public Foo addNullableListMinIntemsItem(java.time.Instant nullableListMinIntemsItem) {
    if (this.nullableListMinIntems == null || !this.nullableListMinIntems.isPresent()) {
      this.nullableListMinIntems = JsonNullable.of(new ArrayList<>());
    }
    this.nullableListMinIntems.get().add(nullableListMinIntemsItem);
    return this;
  }

  /**
   * Get nullableListMinIntems
   * @return nullableListMinIntems
   */
  @Valid @Size(min = 1) 
  @Schema(name = "nullableListMinIntems", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullableListMinIntems")
  @JacksonXmlProperty(localName = "nullableListMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "nullableListMinIntems")
  public JsonNullable<List<java.time.Instant>> getNullableListMinIntems() {
    return nullableListMinIntems;
  }

  public void setNullableListMinIntems(JsonNullable<List<java.time.Instant>> nullableListMinIntems) {
    this.nullableListMinIntems = nullableListMinIntems;
  }

  public Foo requiredDt(java.time.Instant requiredDt) {
    this.requiredDt = requiredDt;
    return this;
  }

  /**
   * Get requiredDt
   * @return requiredDt
   */
  @NotNull @Valid 
  @Schema(name = "requiredDt", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("requiredDt")
  @JacksonXmlProperty(localName = "requiredDt")
  @XmlElement(name = "requiredDt")
  public java.time.Instant getRequiredDt() {
    return requiredDt;
  }

  @JsonProperty("requiredDt")
  @JacksonXmlProperty(localName = "requiredDt")
  public void setRequiredDt(java.time.Instant requiredDt) {
    this.requiredDt = requiredDt;
  }

  public Foo number(java.math.@Nullable BigDecimal number) {
    this.number = number;
    return this;
  }

  /**
   * Get number
   * @return number
   */
  @Valid 
  @Schema(name = "number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("number")
  @JacksonXmlProperty(localName = "number")
  @XmlElement(name = "number")
  public java.math.@Nullable BigDecimal getNumber() {
    return number;
  }

  @JsonProperty("number")
  @JacksonXmlProperty(localName = "number")
  public void setNumber(java.math.@Nullable BigDecimal number) {
    this.number = number;
  }

  public Foo nullableNumber(java.math.@Nullable BigDecimal nullableNumber) {
    this.nullableNumber = JsonNullable.of(nullableNumber);
    return this;
  }

  /**
   * Get nullableNumber
   * @return nullableNumber
   */
  @Valid 
  @Schema(name = "nullableNumber", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullableNumber")
  @JacksonXmlProperty(localName = "nullableNumber")
  @XmlElement(name = "nullableNumber")
  public JsonNullable<java.math.BigDecimal> getNullableNumber() {
    return nullableNumber;
  }

  public void setNullableNumber(JsonNullable<java.math.BigDecimal> nullableNumber) {
    this.nullableNumber = nullableNumber;
  }

  public Foo color(@Nullable String color) {
    this.color = color;
    return this;
  }

  /**
   * Get color
   * @return color
   */
  
  @Schema(name = "color", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  @XmlElement(name = "color")
  public @Nullable String getColor() {
    return color;
  }

  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  public void setColor(@Nullable String color) {
    this.color = color;
  }

  public Foo requiredColor(String requiredColor) {
    this.requiredColor = requiredColor;
    return this;
  }

  /**
   * Get requiredColor
   * @return requiredColor
   */
  @NotNull 
  @Schema(name = "requiredColor", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("requiredColor")
  @JacksonXmlProperty(localName = "requiredColor")
  @XmlElement(name = "requiredColor")
  public String getRequiredColor() {
    return requiredColor;
  }

  @JsonProperty("requiredColor")
  @JacksonXmlProperty(localName = "requiredColor")
  public void setRequiredColor(String requiredColor) {
    this.requiredColor = requiredColor;
  }

  public Foo nullableColor(@Nullable String nullableColor) {
    this.nullableColor = JsonNullable.of(nullableColor);
    return this;
  }

  /**
   * Get nullableColor
   * @return nullableColor
   */
  
  @Schema(name = "nullableColor", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullableColor")
  @JacksonXmlProperty(localName = "nullableColor")
  @XmlElement(name = "nullableColor")
  public JsonNullable<String> getNullableColor() {
    return nullableColor;
  }

  public void setNullableColor(JsonNullable<String> nullableColor) {
    this.nullableColor = nullableColor;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Foo foo = (Foo) o;
    return Objects.equals(this.dt, foo.dt) &&
        equalsNullable(this.nullableDt, foo.nullableDt) &&
        Objects.equals(this.binary, foo.binary) &&
        equalsNullable(this.nullableBinary, foo.nullableBinary) &&
        Objects.equals(this.listOfDt, foo.listOfDt) &&
        Objects.equals(this.listMinIntems, foo.listMinIntems) &&
        equalsNullable(this.nullableListMinIntems, foo.nullableListMinIntems) &&
        Objects.equals(this.requiredDt, foo.requiredDt) &&
        Objects.equals(this.number, foo.number) &&
        equalsNullable(this.nullableNumber, foo.nullableNumber) &&
        Objects.equals(this.color, foo.color) &&
        Objects.equals(this.requiredColor, foo.requiredColor) &&
        equalsNullable(this.nullableColor, foo.nullableColor);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(dt, hashCodeNullable(nullableDt), binary, hashCodeNullable(nullableBinary), listOfDt, listMinIntems, hashCodeNullable(nullableListMinIntems), requiredDt, number, hashCodeNullable(nullableNumber), color, requiredColor, hashCodeNullable(nullableColor));
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Foo {\n");
    sb.append("    dt: ").append(toIndentedString(dt)).append("\n");
    sb.append("    nullableDt: ").append(toIndentedString(nullableDt)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    nullableBinary: ").append(toIndentedString(nullableBinary)).append("\n");
    sb.append("    listOfDt: ").append(toIndentedString(listOfDt)).append("\n");
    sb.append("    listMinIntems: ").append(toIndentedString(listMinIntems)).append("\n");
    sb.append("    nullableListMinIntems: ").append(toIndentedString(nullableListMinIntems)).append("\n");
    sb.append("    requiredDt: ").append(toIndentedString(requiredDt)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    nullableNumber: ").append(toIndentedString(nullableNumber)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    requiredColor: ").append(toIndentedString(requiredColor)).append("\n");
    sb.append("    nullableColor: ").append(toIndentedString(nullableColor)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private Foo instance;

    public Builder() {
      this(new Foo());
    }

    protected Builder(Foo instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Foo value) { 
      this.instance.setDt(value.dt);
      this.instance.setNullableDt(value.nullableDt);
      this.instance.setBinary(value.binary);
      this.instance.setNullableBinary(value.nullableBinary);
      this.instance.setListOfDt(value.listOfDt);
      this.instance.setListMinIntems(value.listMinIntems);
      this.instance.setNullableListMinIntems(value.nullableListMinIntems);
      this.instance.setRequiredDt(value.requiredDt);
      this.instance.setNumber(value.number);
      this.instance.setNullableNumber(value.nullableNumber);
      this.instance.setColor(value.color);
      this.instance.setRequiredColor(value.requiredColor);
      this.instance.setNullableColor(value.nullableColor);
      return this;
    }

    public Foo.Builder dt(java.time.@Nullable Instant dt) {
      this.instance.dt(dt);
      return this;
    }
    
    public Foo.Builder nullableDt(java.time.@Nullable Instant nullableDt) {
      this.instance.nullableDt(nullableDt);
      return this;
    }
    
    public Foo.Builder nullableDt(JsonNullable<java.time.Instant> nullableDt) {
      this.instance.nullableDt = nullableDt;
      return this;
    }
    
    public Foo.Builder binary(org.springframework.core.io.@Nullable Resource binary) {
      this.instance.binary(binary);
      return this;
    }
    
    public Foo.Builder nullableBinary(org.springframework.core.io.@Nullable Resource nullableBinary) {
      this.instance.nullableBinary(nullableBinary);
      return this;
    }
    
    public Foo.Builder nullableBinary(JsonNullable<org.springframework.core.io.Resource> nullableBinary) {
      this.instance.nullableBinary = nullableBinary;
      return this;
    }
    
    public Foo.Builder listOfDt(@Nullable List<java.time.Instant> listOfDt) {
      this.instance.listOfDt(listOfDt);
      return this;
    }
    
    public Foo.Builder listMinIntems(@Nullable List<java.time.Instant> listMinIntems) {
      this.instance.listMinIntems(listMinIntems);
      return this;
    }
    
    public Foo.Builder nullableListMinIntems(@Nullable List<java.time.Instant> nullableListMinIntems) {
      this.instance.nullableListMinIntems(nullableListMinIntems);
      return this;
    }
    
    public Foo.Builder nullableListMinIntems(JsonNullable<List<java.time.Instant>> nullableListMinIntems) {
      this.instance.nullableListMinIntems = nullableListMinIntems;
      return this;
    }
    
    public Foo.Builder requiredDt(java.time.Instant requiredDt) {
      this.instance.requiredDt(requiredDt);
      return this;
    }
    
    public Foo.Builder number(java.math.@Nullable BigDecimal number) {
      this.instance.number(number);
      return this;
    }
    
    public Foo.Builder nullableNumber(java.math.@Nullable BigDecimal nullableNumber) {
      this.instance.nullableNumber(nullableNumber);
      return this;
    }
    
    public Foo.Builder nullableNumber(JsonNullable<java.math.BigDecimal> nullableNumber) {
      this.instance.nullableNumber = nullableNumber;
      return this;
    }
    
    public Foo.Builder color(@Nullable String color) {
      this.instance.color(color);
      return this;
    }
    
    public Foo.Builder requiredColor(String requiredColor) {
      this.instance.requiredColor(requiredColor);
      return this;
    }
    
    public Foo.Builder nullableColor(@Nullable String nullableColor) {
      this.instance.nullableColor(nullableColor);
      return this;
    }
    
    public Foo.Builder nullableColor(JsonNullable<String> nullableColor) {
      this.instance.nullableColor = nullableColor;
      return this;
    }
    
    /**
    * returns a built Foo instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Foo build() {
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
  public static Foo.Builder builder() {
    return new Foo.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Foo.Builder toBuilder() {
    Foo.Builder builder = new Foo.Builder();
    return builder.copyOf(this);
  }

}

