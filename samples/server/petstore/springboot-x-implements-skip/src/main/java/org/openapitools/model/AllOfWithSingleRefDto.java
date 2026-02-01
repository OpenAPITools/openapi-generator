package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.SingleRefTypeDto;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * AllOfWithSingleRefDto
 */

@JsonTypeName("AllOfWithSingleRef")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class AllOfWithSingleRefDto {

  private @Nullable String username;

  private @Nullable SingleRefTypeDto singleRefType;

  public AllOfWithSingleRefDto username(@Nullable String username) {
    this.username = username;
    return this;
  }

  /**
   * Get username
   * @return username
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("username")
  public @Nullable String getUsername() {
    return username;
  }

  public void setUsername(@Nullable String username) {
    this.username = username;
  }

  public AllOfWithSingleRefDto singleRefType(@Nullable SingleRefTypeDto singleRefType) {
    this.singleRefType = singleRefType;
    return this;
  }

  /**
   * Get singleRefType
   * @return singleRefType
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("SingleRefType")
  public @Nullable SingleRefTypeDto getSingleRefType() {
    return singleRefType;
  }

  public void setSingleRefType(@Nullable SingleRefTypeDto singleRefType) {
    this.singleRefType = singleRefType;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AllOfWithSingleRefDto allOfWithSingleRef = (AllOfWithSingleRefDto) o;
    return Objects.equals(this.username, allOfWithSingleRef.username) &&
        Objects.equals(this.singleRefType, allOfWithSingleRef.singleRefType);
  }

  @Override
  public int hashCode() {
    return Objects.hash(username, singleRefType);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AllOfWithSingleRefDto {\n");
    sb.append("    username: ").append(toIndentedString(username)).append("\n");
    sb.append("    singleRefType: ").append(toIndentedString(singleRefType)).append("\n");
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

