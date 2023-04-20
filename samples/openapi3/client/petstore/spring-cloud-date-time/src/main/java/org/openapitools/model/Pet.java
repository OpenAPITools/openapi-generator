package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Pet
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Pet {

  private String atType = "Pet";

  private Integer age = 4;

  private Boolean happy = true;

  private BigDecimal price = new BigDecimal("32000000000");

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime lastFeed = OffsetDateTime.parse("1973-12-19T11:39:57Z[UTC]", java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME.withZone(java.time.ZoneId.systemDefault()));

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private LocalDate dateOfBirth = LocalDate.parse("2021-01-01");

  /**
   * Default constructor
   * @deprecated Use {@link Pet#Pet(String)}
   */
  @Deprecated
  public Pet() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Pet(String atType) {
    this.atType = atType;
  }

  public Pet atType(String atType) {
    this.atType = atType;
    return this;
  }

  /**
   * Get atType
   * @return atType
  */
  @NotNull 
  @Schema(name = "@type", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("@type")
  public String getAtType() {
    return atType;
  }

  public void setAtType(String atType) {
    this.atType = atType;
  }

  public Pet age(Integer age) {
    this.age = age;
    return this;
  }

  /**
   * Get age
   * @return age
  */
  
  @Schema(name = "age", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("age")
  public Integer getAge() {
    return age;
  }

  public void setAge(Integer age) {
    this.age = age;
  }

  public Pet happy(Boolean happy) {
    this.happy = happy;
    return this;
  }

  /**
   * Get happy
   * @return happy
  */
  
  @Schema(name = "happy", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("happy")
  public Boolean getHappy() {
    return happy;
  }

  public void setHappy(Boolean happy) {
    this.happy = happy;
  }

  public Pet price(BigDecimal price) {
    this.price = price;
    return this;
  }

  /**
   * Get price
   * @return price
  */
  @Valid 
  @Schema(name = "price", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("price")
  public BigDecimal getPrice() {
    return price;
  }

  public void setPrice(BigDecimal price) {
    this.price = price;
  }

  public Pet lastFeed(OffsetDateTime lastFeed) {
    this.lastFeed = lastFeed;
    return this;
  }

  /**
   * Get lastFeed
   * @return lastFeed
  */
  @Valid 
  @Schema(name = "lastFeed", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("lastFeed")
  public OffsetDateTime getLastFeed() {
    return lastFeed;
  }

  public void setLastFeed(OffsetDateTime lastFeed) {
    this.lastFeed = lastFeed;
  }

  public Pet dateOfBirth(LocalDate dateOfBirth) {
    this.dateOfBirth = dateOfBirth;
    return this;
  }

  /**
   * Get dateOfBirth
   * @return dateOfBirth
  */
  @Valid 
  @Schema(name = "dateOfBirth", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dateOfBirth")
  public LocalDate getDateOfBirth() {
    return dateOfBirth;
  }

  public void setDateOfBirth(LocalDate dateOfBirth) {
    this.dateOfBirth = dateOfBirth;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Pet pet = (Pet) o;
    return Objects.equals(this.atType, pet.atType) &&
        Objects.equals(this.age, pet.age) &&
        Objects.equals(this.happy, pet.happy) &&
        Objects.equals(this.price, pet.price) &&
        Objects.equals(this.lastFeed, pet.lastFeed) &&
        Objects.equals(this.dateOfBirth, pet.dateOfBirth);
  }

  @Override
  public int hashCode() {
    return Objects.hash(atType, age, happy, price, lastFeed, dateOfBirth);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    sb.append("    atType: ").append(toIndentedString(atType)).append("\n");
    sb.append("    age: ").append(toIndentedString(age)).append("\n");
    sb.append("    happy: ").append(toIndentedString(happy)).append("\n");
    sb.append("    price: ").append(toIndentedString(price)).append("\n");
    sb.append("    lastFeed: ").append(toIndentedString(lastFeed)).append("\n");
    sb.append("    dateOfBirth: ").append(toIndentedString(dateOfBirth)).append("\n");
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

