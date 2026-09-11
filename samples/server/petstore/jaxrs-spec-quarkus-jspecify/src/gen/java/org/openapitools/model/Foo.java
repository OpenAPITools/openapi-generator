package org.openapitools.model;

import java.io.File;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("Foo")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class Foo   {
  private @Nullable OffsetDateTime dt;
  private @Nullable OffsetDateTime nullableDt;
  private @Nullable File binary;
  private @Nullable File nullableBinary;
  private @Valid @Nullable List<OffsetDateTime> listOfDt = new ArrayList<>();
  private @Valid @Nullable List<OffsetDateTime> listMinIntems = new ArrayList<>();
  private @Valid @Nullable List<OffsetDateTime> nullableListMinIntems;
  private OffsetDateTime requiredDt;
  private java.math.@Nullable BigDecimal number;
  private java.math.@Nullable BigDecimal nullableNumber;
  private @Nullable String color = "red";
  private String requiredColor = "red";
  private @Nullable String nullableColor = "red";

  public Foo() {
  }

  @JsonCreator
  public Foo(
    @JsonProperty(required = true, value = "requiredDt") OffsetDateTime requiredDt,
    @JsonProperty(required = true, value = "requiredColor") String requiredColor
  ) {
    this.requiredDt = requiredDt;
    this.requiredColor = requiredColor;
  }

  /**
   **/
  public Foo dt(@Nullable OffsetDateTime dt) {
    this.dt = dt;
    return this;
  }

  
  @JsonProperty("dt")
  public @Nullable OffsetDateTime getDt() {
    return dt;
  }

  @JsonProperty("dt")
  public void setDt(@Nullable OffsetDateTime dt) {
    this.dt = dt;
  }

  /**
   **/
  public Foo nullableDt(@Nullable OffsetDateTime nullableDt) {
    this.nullableDt = nullableDt;
    return this;
  }

  
  @JsonProperty("nullableDt")
  public @Nullable OffsetDateTime getNullableDt() {
    return nullableDt;
  }

  @JsonProperty("nullableDt")
  public void setNullableDt(@Nullable OffsetDateTime nullableDt) {
    this.nullableDt = nullableDt;
  }

  /**
   **/
  public Foo binary(@Nullable File binary) {
    this.binary = binary;
    return this;
  }

  
  @JsonProperty("binary")
  public @Nullable File getBinary() {
    return binary;
  }

  @JsonProperty("binary")
  public void setBinary(@Nullable File binary) {
    this.binary = binary;
  }

  /**
   **/
  public Foo nullableBinary(@Nullable File nullableBinary) {
    this.nullableBinary = nullableBinary;
    return this;
  }

  
  @JsonProperty("nullableBinary")
  public @Nullable File getNullableBinary() {
    return nullableBinary;
  }

  @JsonProperty("nullableBinary")
  public void setNullableBinary(@Nullable File nullableBinary) {
    this.nullableBinary = nullableBinary;
  }

  /**
   **/
  public Foo listOfDt(@Nullable List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
    return this;
  }

  
  @JsonProperty("listOfDt")
  public @Nullable List<OffsetDateTime> getListOfDt() {
    return listOfDt;
  }

  @JsonProperty("listOfDt")
  public void setListOfDt(@Nullable List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
  }

  public Foo addListOfDtItem(OffsetDateTime listOfDtItem) {
    if (this.listOfDt == null) {
      this.listOfDt = new ArrayList<>();
    }

    this.listOfDt.add(listOfDtItem);
    return this;
  }

  public Foo removeListOfDtItem(OffsetDateTime listOfDtItem) {
    if (listOfDtItem != null && this.listOfDt != null) {
      this.listOfDt.remove(listOfDtItem);
    }

    return this;
  }
  /**
   **/
  public Foo listMinIntems(@Nullable List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
    return this;
  }

  
  @JsonProperty("listMinIntems")
   @Size(min=1)public @Nullable List<OffsetDateTime> getListMinIntems() {
    return listMinIntems;
  }

  @JsonProperty("listMinIntems")
  public void setListMinIntems(@Nullable List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
  }

  public Foo addListMinIntemsItem(OffsetDateTime listMinIntemsItem) {
    if (this.listMinIntems == null) {
      this.listMinIntems = new ArrayList<>();
    }

    this.listMinIntems.add(listMinIntemsItem);
    return this;
  }

  public Foo removeListMinIntemsItem(OffsetDateTime listMinIntemsItem) {
    if (listMinIntemsItem != null && this.listMinIntems != null) {
      this.listMinIntems.remove(listMinIntemsItem);
    }

    return this;
  }
  /**
   **/
  public Foo nullableListMinIntems(@Nullable List<OffsetDateTime> nullableListMinIntems) {
    this.nullableListMinIntems = nullableListMinIntems;
    return this;
  }

  
  @JsonProperty("nullableListMinIntems")
   @Size(min=1)public @Nullable List<OffsetDateTime> getNullableListMinIntems() {
    return nullableListMinIntems;
  }

  @JsonProperty("nullableListMinIntems")
  public void setNullableListMinIntems(@Nullable List<OffsetDateTime> nullableListMinIntems) {
    this.nullableListMinIntems = nullableListMinIntems;
  }

  public Foo addNullableListMinIntemsItem(OffsetDateTime nullableListMinIntemsItem) {
    if (this.nullableListMinIntems == null) {
      this.nullableListMinIntems = new ArrayList<>();
    }

    this.nullableListMinIntems.add(nullableListMinIntemsItem);
    return this;
  }

  public Foo removeNullableListMinIntemsItem(OffsetDateTime nullableListMinIntemsItem) {
    if (nullableListMinIntemsItem != null && this.nullableListMinIntems != null) {
      this.nullableListMinIntems.remove(nullableListMinIntemsItem);
    }

    return this;
  }
  /**
   **/
  public Foo requiredDt(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
    return this;
  }

  
  @JsonProperty(required = true, value = "requiredDt")
  @NotNull public OffsetDateTime getRequiredDt() {
    return requiredDt;
  }

  @JsonProperty(required = true, value = "requiredDt")
  public void setRequiredDt(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
  }

  /**
   **/
  public Foo number(java.math.@Nullable BigDecimal number) {
    this.number = number;
    return this;
  }

  
  @JsonProperty("number")
  @Valid public java.math.@Nullable BigDecimal getNumber() {
    return number;
  }

  @JsonProperty("number")
  public void setNumber(java.math.@Nullable BigDecimal number) {
    this.number = number;
  }

  /**
   **/
  public Foo nullableNumber(java.math.@Nullable BigDecimal nullableNumber) {
    this.nullableNumber = nullableNumber;
    return this;
  }

  
  @JsonProperty("nullableNumber")
  @Valid public java.math.@Nullable BigDecimal getNullableNumber() {
    return nullableNumber;
  }

  @JsonProperty("nullableNumber")
  public void setNullableNumber(java.math.@Nullable BigDecimal nullableNumber) {
    this.nullableNumber = nullableNumber;
  }

  /**
   **/
  public Foo color(@Nullable String color) {
    this.color = color;
    return this;
  }

  
  @JsonProperty("color")
  public @Nullable String getColor() {
    return color;
  }

  @JsonProperty("color")
  public void setColor(@Nullable String color) {
    this.color = color;
  }

  /**
   **/
  public Foo requiredColor(String requiredColor) {
    this.requiredColor = requiredColor;
    return this;
  }

  
  @JsonProperty(required = true, value = "requiredColor")
  @NotNull public String getRequiredColor() {
    return requiredColor;
  }

  @JsonProperty(required = true, value = "requiredColor")
  public void setRequiredColor(String requiredColor) {
    this.requiredColor = requiredColor;
  }

  /**
   **/
  public Foo nullableColor(@Nullable String nullableColor) {
    this.nullableColor = nullableColor;
    return this;
  }

  
  @JsonProperty("nullableColor")
  public @Nullable String getNullableColor() {
    return nullableColor;
  }

  @JsonProperty("nullableColor")
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


}
