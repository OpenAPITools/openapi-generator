package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.DeprecatedObjectDto;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ObjectWithDeprecatedFieldsDto
 */

@JsonTypeName("ObjectWithDeprecatedFields")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class ObjectWithDeprecatedFieldsDto {

  private @Nullable String uuid;

  @Deprecated
  private @Nullable BigDecimal id;

  @Deprecated
  private @Nullable DeprecatedObjectDto deprecatedRef;

  @Deprecated
  @Valid
  private List<String> bars = new ArrayList<>();

  public ObjectWithDeprecatedFieldsDto uuid(@Nullable String uuid) {
    this.uuid = uuid;
    return this;
  }

  /**
   * Get uuid
   * @return uuid
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public @Nullable String getUuid() {
    return uuid;
  }

  public void setUuid(@Nullable String uuid) {
    this.uuid = uuid;
  }

  public ObjectWithDeprecatedFieldsDto id(@Nullable BigDecimal id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
   * @deprecated
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  @Deprecated
  public @Nullable BigDecimal getId() {
    return id;
  }

  /**
   * @deprecated
   */
  @Deprecated
  public void setId(@Nullable BigDecimal id) {
    this.id = id;
  }

  public ObjectWithDeprecatedFieldsDto deprecatedRef(@Nullable DeprecatedObjectDto deprecatedRef) {
    this.deprecatedRef = deprecatedRef;
    return this;
  }

  /**
   * Get deprecatedRef
   * @return deprecatedRef
   * @deprecated
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("deprecatedRef")
  @Deprecated
  public @Nullable DeprecatedObjectDto getDeprecatedRef() {
    return deprecatedRef;
  }

  /**
   * @deprecated
   */
  @Deprecated
  public void setDeprecatedRef(@Nullable DeprecatedObjectDto deprecatedRef) {
    this.deprecatedRef = deprecatedRef;
  }

  public ObjectWithDeprecatedFieldsDto bars(List<String> bars) {
    this.bars = bars;
    return this;
  }

  public ObjectWithDeprecatedFieldsDto addBarsItem(String barsItem) {
    if (this.bars == null) {
      this.bars = new ArrayList<>();
    }
    this.bars.add(barsItem);
    return this;
  }

  /**
   * Get bars
   * @return bars
   * @deprecated
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("bars")
  @Deprecated
  public List<String> getBars() {
    return bars;
  }

  /**
   * @deprecated
   */
  @Deprecated
  public void setBars(List<String> bars) {
    this.bars = bars;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ObjectWithDeprecatedFieldsDto objectWithDeprecatedFields = (ObjectWithDeprecatedFieldsDto) o;
    return Objects.equals(this.uuid, objectWithDeprecatedFields.uuid) &&
        Objects.equals(this.id, objectWithDeprecatedFields.id) &&
        Objects.equals(this.deprecatedRef, objectWithDeprecatedFields.deprecatedRef) &&
        Objects.equals(this.bars, objectWithDeprecatedFields.bars);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, id, deprecatedRef, bars);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ObjectWithDeprecatedFieldsDto {\n");
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    deprecatedRef: ").append(toIndentedString(deprecatedRef)).append("\n");
    sb.append("    bars: ").append(toIndentedString(bars)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

