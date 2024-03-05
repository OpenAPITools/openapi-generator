package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.format.annotation.DateTimeFormat;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * ObjectWithUniqueItems
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ObjectWithUniqueItems {

  @Valid
  private JsonNullable<Set<String>> nullSet = JsonNullable.<Set<String>>undefined();

  @Valid
  private Set<String> notNullSet;

  @Valid
  private JsonNullable<List<String>> nullList = JsonNullable.<List<String>>undefined();

  @Valid
  private List<String> notNullList;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime notNullDateField;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime nullDateField;

  public ObjectWithUniqueItems nullSet(Set<String> nullSet) {
    this.nullSet = JsonNullable.of(nullSet);
    return this;
  }

  public ObjectWithUniqueItems addNullSetItem(String nullSetItem) {
    if (this.nullSet == null || !this.nullSet.isPresent()) {
      this.nullSet = JsonNullable.of(new LinkedHashSet<>());
    }
    this.nullSet.get().add(nullSetItem);
    return this;
  }

  /**
   * Get nullSet
   * @return nullSet
  */
  
  @Schema(name = "nullSet", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullSet")
  public JsonNullable<Set<String>> getNullSet() {
    return nullSet;
  }

  public void setNullSet(JsonNullable<Set<String>> nullSet) {
    this.nullSet = nullSet;
  }

  public ObjectWithUniqueItems notNullSet(Set<String> notNullSet) {
    this.notNullSet = notNullSet;
    return this;
  }

  public ObjectWithUniqueItems addNotNullSetItem(String notNullSetItem) {
    if (this.notNullSet == null) {
      this.notNullSet = new LinkedHashSet<>();
    }
    this.notNullSet.add(notNullSetItem);
    return this;
  }

  /**
   * Get notNullSet
   * @return notNullSet
  */
  
  @Schema(name = "notNullSet", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("notNullSet")
  public Set<String> getNotNullSet() {
    return notNullSet;
  }

  @JsonDeserialize(as = LinkedHashSet.class)
  public void setNotNullSet(Set<String> notNullSet) {
    this.notNullSet = notNullSet;
  }

  public ObjectWithUniqueItems nullList(List<String> nullList) {
    this.nullList = JsonNullable.of(nullList);
    return this;
  }

  public ObjectWithUniqueItems addNullListItem(String nullListItem) {
    if (this.nullList == null || !this.nullList.isPresent()) {
      this.nullList = JsonNullable.of(new ArrayList<>());
    }
    this.nullList.get().add(nullListItem);
    return this;
  }

  /**
   * Get nullList
   * @return nullList
  */
  
  @Schema(name = "nullList", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullList")
  public JsonNullable<List<String>> getNullList() {
    return nullList;
  }

  public void setNullList(JsonNullable<List<String>> nullList) {
    this.nullList = nullList;
  }

  public ObjectWithUniqueItems notNullList(List<String> notNullList) {
    this.notNullList = notNullList;
    return this;
  }

  public ObjectWithUniqueItems addNotNullListItem(String notNullListItem) {
    if (this.notNullList == null) {
      this.notNullList = new ArrayList<>();
    }
    this.notNullList.add(notNullListItem);
    return this;
  }

  /**
   * Get notNullList
   * @return notNullList
  */
  
  @Schema(name = "notNullList", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("notNullList")
  public List<String> getNotNullList() {
    return notNullList;
  }

  public void setNotNullList(List<String> notNullList) {
    this.notNullList = notNullList;
  }

  public ObjectWithUniqueItems notNullDateField(OffsetDateTime notNullDateField) {
    this.notNullDateField = notNullDateField;
    return this;
  }

  /**
   * Get notNullDateField
   * @return notNullDateField
  */
  @Valid 
  @Schema(name = "notNullDateField", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("notNullDateField")
  public OffsetDateTime getNotNullDateField() {
    return notNullDateField;
  }

  public void setNotNullDateField(OffsetDateTime notNullDateField) {
    this.notNullDateField = notNullDateField;
  }

  public ObjectWithUniqueItems nullDateField(OffsetDateTime nullDateField) {
    this.nullDateField = nullDateField;
    return this;
  }

  /**
   * Get nullDateField
   * @return nullDateField
  */
  @Valid 
  @Schema(name = "nullDateField", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("nullDateField")
  public OffsetDateTime getNullDateField() {
    return nullDateField;
  }

  public void setNullDateField(OffsetDateTime nullDateField) {
    this.nullDateField = nullDateField;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ObjectWithUniqueItems objectWithUniqueItems = (ObjectWithUniqueItems) o;
    return equalsNullable(this.nullSet, objectWithUniqueItems.nullSet) &&
        Objects.equals(this.notNullSet, objectWithUniqueItems.notNullSet) &&
        equalsNullable(this.nullList, objectWithUniqueItems.nullList) &&
        Objects.equals(this.notNullList, objectWithUniqueItems.notNullList) &&
        Objects.equals(this.notNullDateField, objectWithUniqueItems.notNullDateField) &&
        Objects.equals(this.nullDateField, objectWithUniqueItems.nullDateField);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(hashCodeNullable(nullSet), notNullSet, hashCodeNullable(nullList), notNullList, notNullDateField, nullDateField);
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
    sb.append("class ObjectWithUniqueItems {\n");
    sb.append("    nullSet: ").append(toIndentedString(nullSet)).append("\n");
    sb.append("    notNullSet: ").append(toIndentedString(notNullSet)).append("\n");
    sb.append("    nullList: ").append(toIndentedString(nullList)).append("\n");
    sb.append("    notNullList: ").append(toIndentedString(notNullList)).append("\n");
    sb.append("    notNullDateField: ").append(toIndentedString(notNullDateField)).append("\n");
    sb.append("    nullDateField: ").append(toIndentedString(nullDateField)).append("\n");
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

