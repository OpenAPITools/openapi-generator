package org.openapitools.virtualan.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import java.math.BigDecimal;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * OuterComposite
 */

@JsonPropertyOrder({
    OuterComposite.JSON_PROPERTY_MY_NUMBER,
    OuterComposite.JSON_PROPERTY_MY_STRING,
    OuterComposite.JSON_PROPERTY_MY_BOOLEAN
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class OuterComposite {

    public static final String JSON_PROPERTY_MY_NUMBER = "my_number";
  private @Nullable BigDecimal myNumber;

    public static final String JSON_PROPERTY_MY_STRING = "my_string";
  private @Nullable String myString;

    public static final String JSON_PROPERTY_MY_BOOLEAN = "my_boolean";
  private @Nullable Boolean myBoolean;

  public OuterComposite myNumber(@Nullable BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

  /**
   * Get myNumber
   * @return myNumber
   */
  @Valid 
  @Schema(name = "my_number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_number")
  public @Nullable BigDecimal getMyNumber() {
    return myNumber;
  }

  @JsonProperty("my_number")
  public void setMyNumber(@Nullable BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  public OuterComposite myString(@Nullable String myString) {
    this.myString = myString;
    return this;
  }

  /**
   * Get myString
   * @return myString
   */
  
  @Schema(name = "my_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_string")
  public @Nullable String getMyString() {
    return myString;
  }

  @JsonProperty("my_string")
  public void setMyString(@Nullable String myString) {
    this.myString = myString;
  }

  public OuterComposite myBoolean(@Nullable Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
  }

  /**
   * Get myBoolean
   * @return myBoolean
   */
  
  @Schema(name = "my_boolean", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_boolean")
  public @Nullable Boolean getMyBoolean() {
    return myBoolean;
  }

  @JsonProperty("my_boolean")
  public void setMyBoolean(@Nullable Boolean myBoolean) {
    this.myBoolean = myBoolean;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterComposite outerComposite = (OuterComposite) o;
    return Objects.equals(this.myNumber, outerComposite.myNumber) &&
        Objects.equals(this.myString, outerComposite.myString) &&
        Objects.equals(this.myBoolean, outerComposite.myBoolean);
  }

  @Override
  public int hashCode() {
    return Objects.hash(myNumber, myString, myBoolean);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OuterComposite {\n");
    sb.append("    myNumber: ").append(toIndentedString(myNumber)).append("\n");
    sb.append("    myString: ").append(toIndentedString(myString)).append("\n");
    sb.append("    myBoolean: ").append(toIndentedString(myBoolean)).append("\n");
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
}

