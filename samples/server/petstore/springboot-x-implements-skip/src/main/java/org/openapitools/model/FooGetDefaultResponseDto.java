package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.FooDto;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * FooGetDefaultResponseDto
 */

@JsonTypeName("_foo_get_default_response")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.21.0-SNAPSHOT")
public class FooGetDefaultResponseDto {

  private @Nullable FooDto string;

  public FooGetDefaultResponseDto string(@Nullable FooDto string) {
    this.string = string;
    return this;
  }

  /**
   * Get string
   * @return string
   */
  @Valid 
  @Schema(name = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("string")
  public @Nullable FooDto getString() {
    return string;
  }

  @JsonProperty("string")
  public void setString(@Nullable FooDto string) {
    this.string = string;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FooGetDefaultResponseDto fooGetDefaultResponse = (FooGetDefaultResponseDto) o;
    return Objects.equals(this.string, fooGetDefaultResponse.string);
  }

  @Override
  public int hashCode() {
    return Objects.hash(string);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FooGetDefaultResponseDto {\n");
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
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

