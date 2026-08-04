package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.annotation.Nulls;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
import org.springframework.format.annotation.DateTimeFormat;
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
  private @Nullable OffsetDateTime dt;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime nullableDt = null;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private org.springframework.core.io.@Nullable Resource binary;

  private org.springframework.core.io.@Nullable Resource nullableBinary = null;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<OffsetDateTime> listOfDt = new ArrayList<>();

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<OffsetDateTime> listMinIntems = new ArrayList<>();

  private @Nullable List<OffsetDateTime> nullableListMinIntems;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime requiredDt;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal number;

  private @Nullable BigDecimal nullableNumber = null;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String color = "red";

  private String requiredColor = "red";

  private @Nullable String nullableColor = null;

  public Foo() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Foo(OffsetDateTime requiredDt, String requiredColor) {
    this.requiredDt = requiredDt;
    this.requiredColor = requiredColor;
  }

  /**
   * Constructor with all args parameters
   */
  public Foo(OffsetDateTime dt, OffsetDateTime nullableDt, org.springframework.core.io.Resource binary, org.springframework.core.io.Resource nullableBinary, List<OffsetDateTime> listOfDt, List<OffsetDateTime> listMinIntems, List<OffsetDateTime> nullableListMinIntems, OffsetDateTime requiredDt, BigDecimal number, BigDecimal nullableNumber, String color, String requiredColor, String nullableColor) {
      this.dt = dt;
      this.nullableDt = nullableDt;
      this.binary = binary;
      this.nullableBinary = nullableBinary;
      this.listOfDt = listOfDt;
      this.listMinIntems = listMinIntems;
      this.nullableListMinIntems = nullableListMinIntems;
      this.requiredDt = requiredDt;
      this.number = number;
      this.nullableNumber = nullableNumber;
      this.color = color;
      this.requiredColor = requiredColor;
      this.nullableColor = nullableColor;
  }

  public Foo dt(OffsetDateTime dt) {
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
  public @Nullable OffsetDateTime getDt() {
    return dt;
  }

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("dt")
  @JacksonXmlProperty(localName = "dt")
  public void setDt(@Nullable OffsetDateTime dt) {
    this.dt = dt;
  }

  public Foo nullableDt(OffsetDateTime nullableDt) {
    this.nullableDt = nullableDt;
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
  public @Nullable OffsetDateTime getNullableDt() {
    return nullableDt;
  }

  @JsonProperty("nullableDt")
  @JacksonXmlProperty(localName = "nullableDt")
  public void setNullableDt(@Nullable OffsetDateTime nullableDt) {
    this.nullableDt = nullableDt;
  }

  public Foo binary(org.springframework.core.io.Resource binary) {
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

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("binary")
  @JacksonXmlProperty(localName = "binary")
  public void setBinary(org.springframework.core.io.@Nullable Resource binary) {
    this.binary = binary;
  }

  public Foo nullableBinary(org.springframework.core.io.Resource nullableBinary) {
    this.nullableBinary = nullableBinary;
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
  public org.springframework.core.io.@Nullable Resource getNullableBinary() {
    return nullableBinary;
  }

  @JsonProperty("nullableBinary")
  @JacksonXmlProperty(localName = "nullableBinary")
  public void setNullableBinary(org.springframework.core.io.@Nullable Resource nullableBinary) {
    this.nullableBinary = nullableBinary;
  }

  public Foo listOfDt(List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
    return this;
  }

  public Foo addListOfDtItem(OffsetDateTime listOfDtItem) {
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
  public List<OffsetDateTime> getListOfDt() {
    return listOfDt;
  }

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("listOfDt")
  @JacksonXmlProperty(localName = "listOfDt")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListOfDt(List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
  }

  public Foo listMinIntems(List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
    return this;
  }

  public Foo addListMinIntemsItem(OffsetDateTime listMinIntemsItem) {
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
  public List<OffsetDateTime> getListMinIntems() {
    return listMinIntems;
  }

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("listMinIntems")
  @JacksonXmlProperty(localName = "listMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListMinIntems(List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
  }

  public Foo nullableListMinIntems(List<OffsetDateTime> nullableListMinIntems) {
    this.nullableListMinIntems = nullableListMinIntems;
    return this;
  }

  public Foo addNullableListMinIntemsItem(OffsetDateTime nullableListMinIntemsItem) {
    if (this.nullableListMinIntems == null) {
      this.nullableListMinIntems = new ArrayList<>();
    }
    this.nullableListMinIntems.add(nullableListMinIntemsItem);
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
  public @Nullable List<OffsetDateTime> getNullableListMinIntems() {
    return nullableListMinIntems;
  }

  @JsonProperty("nullableListMinIntems")
  @JacksonXmlProperty(localName = "nullableListMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setNullableListMinIntems(@Nullable List<OffsetDateTime> nullableListMinIntems) {
    this.nullableListMinIntems = nullableListMinIntems;
  }

  public Foo requiredDt(OffsetDateTime requiredDt) {
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
  public OffsetDateTime getRequiredDt() {
    return requiredDt;
  }

  @JsonProperty("requiredDt")
  @JacksonXmlProperty(localName = "requiredDt")
  public void setRequiredDt(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
  }

  public Foo number(BigDecimal number) {
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
  public @Nullable BigDecimal getNumber() {
    return number;
  }

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("number")
  @JacksonXmlProperty(localName = "number")
  public void setNumber(@Nullable BigDecimal number) {
    this.number = number;
  }

  public Foo nullableNumber(BigDecimal nullableNumber) {
    this.nullableNumber = nullableNumber;
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
  public @Nullable BigDecimal getNullableNumber() {
    return nullableNumber;
  }

  @JsonProperty("nullableNumber")
  @JacksonXmlProperty(localName = "nullableNumber")
  public void setNullableNumber(@Nullable BigDecimal nullableNumber) {
    this.nullableNumber = nullableNumber;
  }

  public Foo color(String color) {
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
  public String getColor() {
    return color;
  }

  @JsonSetter(nulls = Nulls.SKIP)
  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  public void setColor(String color) {
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

  public Foo nullableColor(String nullableColor) {
    this.nullableColor = nullableColor;
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
  public @Nullable String getNullableColor() {
    return nullableColor;
  }

  @JsonProperty("nullableColor")
  @JacksonXmlProperty(localName = "nullableColor")
  public void setNullableColor(@Nullable String nullableColor) {
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
        Objects.equals(this.nullableDt, foo.nullableDt) &&
        Objects.equals(this.binary, foo.binary) &&
        Objects.equals(this.nullableBinary, foo.nullableBinary) &&
        Objects.equals(this.listOfDt, foo.listOfDt) &&
        Objects.equals(this.listMinIntems, foo.listMinIntems) &&
        Objects.equals(this.nullableListMinIntems, foo.nullableListMinIntems) &&
        Objects.equals(this.requiredDt, foo.requiredDt) &&
        Objects.equals(this.number, foo.number) &&
        Objects.equals(this.nullableNumber, foo.nullableNumber) &&
        Objects.equals(this.color, foo.color) &&
        Objects.equals(this.requiredColor, foo.requiredColor) &&
        Objects.equals(this.nullableColor, foo.nullableColor);
  }

  @Override
  public int hashCode() {
    return Objects.hash(dt, nullableDt, binary, nullableBinary, listOfDt, listMinIntems, nullableListMinIntems, requiredDt, number, nullableNumber, color, requiredColor, nullableColor);
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
  private String toIndentedString(Object o) {
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

    public Foo.Builder dt(OffsetDateTime dt) {
      this.instance.dt(dt);
      return this;
    }
    
    public Foo.Builder nullableDt(OffsetDateTime nullableDt) {
      this.instance.nullableDt(nullableDt);
      return this;
    }
    
    public Foo.Builder binary(org.springframework.core.io.Resource binary) {
      this.instance.binary(binary);
      return this;
    }
    
    public Foo.Builder nullableBinary(org.springframework.core.io.Resource nullableBinary) {
      this.instance.nullableBinary(nullableBinary);
      return this;
    }
    
    public Foo.Builder listOfDt(List<OffsetDateTime> listOfDt) {
      this.instance.listOfDt(listOfDt);
      return this;
    }
    
    public Foo.Builder listMinIntems(List<OffsetDateTime> listMinIntems) {
      this.instance.listMinIntems(listMinIntems);
      return this;
    }
    
    public Foo.Builder nullableListMinIntems(List<OffsetDateTime> nullableListMinIntems) {
      this.instance.nullableListMinIntems(nullableListMinIntems);
      return this;
    }
    
    public Foo.Builder requiredDt(OffsetDateTime requiredDt) {
      this.instance.requiredDt(requiredDt);
      return this;
    }
    
    public Foo.Builder number(BigDecimal number) {
      this.instance.number(number);
      return this;
    }
    
    public Foo.Builder nullableNumber(BigDecimal nullableNumber) {
      this.instance.nullableNumber(nullableNumber);
      return this;
    }
    
    public Foo.Builder color(String color) {
      this.instance.color(color);
      return this;
    }
    
    public Foo.Builder requiredColor(String requiredColor) {
      this.instance.requiredColor(requiredColor);
      return this;
    }
    
    public Foo.Builder nullableColor(String nullableColor) {
      this.instance.nullableColor(nullableColor);
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

