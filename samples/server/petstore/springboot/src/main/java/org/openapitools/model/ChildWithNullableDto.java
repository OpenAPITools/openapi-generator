package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Arrays;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.ParentWithNullableDto;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ChildWithNullableDto
 */


@JsonTypeName("ChildWithNullable")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ChildWithNullableDto extends ParentWithNullableDto {

  private String otherProperty;

  public ChildWithNullableDto otherProperty(String otherProperty) {
    this.otherProperty = otherProperty;
    return this;
  }

  /**
   * Get otherProperty
   * @return otherProperty
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("otherProperty")
  public String getOtherProperty() {
    return otherProperty;
  }

  public void setOtherProperty(String otherProperty) {
    this.otherProperty = otherProperty;
  }


  public ChildWithNullableDto type(TypeEnum type) {
    super.type(type);
    return this;
  }

  public ChildWithNullableDto nullableProperty(String nullableProperty) {
    super.nullableProperty(nullableProperty);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ChildWithNullableDto childWithNullable = (ChildWithNullableDto) o;
    return Objects.equals(this.otherProperty, childWithNullable.otherProperty) &&
        super.equals(o);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(otherProperty, super.hashCode());
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
    sb.append("class ChildWithNullableDto {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    otherProperty: ").append(toIndentedString(otherProperty)).append("\n");
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

