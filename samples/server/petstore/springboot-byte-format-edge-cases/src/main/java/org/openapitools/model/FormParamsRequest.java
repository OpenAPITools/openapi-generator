package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.Arrays;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * FormParamsRequest
 */

@JsonTypeName("formParams_request")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class FormParamsRequest {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String plain;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable byte[] bytes;

  public FormParamsRequest plain(@Nullable String plain) {
    this.plain = plain;
    return this;
  }

  /**
   * Get plain
   * @return plain
   */
  
  @Schema(name = "plain", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("plain")
  public @Nullable String getPlain() {
    return plain;
  }

  @JsonProperty("plain")
  public void setPlain(@Nullable String plain) {
    this.plain = plain;
  }

  public FormParamsRequest bytes(@Nullable byte[] bytes) {
    this.bytes = bytes;
    return this;
  }

  /**
   * Get bytes
   * @return bytes
   */
  
  @Schema(name = "bytes", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("bytes")
  public @Nullable byte[] getBytes() {
    return bytes;
  }

  @JsonProperty("bytes")
  public void setBytes(@Nullable byte[] bytes) {
    this.bytes = bytes;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FormParamsRequest formParamsRequest = (FormParamsRequest) o;
    return Objects.equals(this.plain, formParamsRequest.plain) &&
        Arrays.equals(this.bytes, formParamsRequest.bytes);
  }

  @Override
  public int hashCode() {
    return Objects.hash(plain, Arrays.hashCode(bytes));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FormParamsRequest {\n");
    sb.append("    plain: ").append(toIndentedString(plain)).append("\n");
    sb.append("    bytes: ").append(toIndentedString(bytes)).append("\n");
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

