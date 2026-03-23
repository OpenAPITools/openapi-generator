package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * FakeBigDecimalMap200ResponseDto
 */

@JsonTypeName("fakeBigDecimalMap_200_response")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.21.0-SNAPSHOT")
public class FakeBigDecimalMap200ResponseDto {

  private @Nullable BigDecimal someId;

  @Valid
  private Map<String, BigDecimal> someMap = new HashMap<>();

  public FakeBigDecimalMap200ResponseDto someId(@Nullable BigDecimal someId) {
    this.someId = someId;
    return this;
  }

  /**
   * Get someId
   * @return someId
   */
  @Valid 
  @Schema(name = "someId", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("someId")
  public @Nullable BigDecimal getSomeId() {
    return someId;
  }

  @JsonProperty("someId")
  public void setSomeId(@Nullable BigDecimal someId) {
    this.someId = someId;
  }

  public FakeBigDecimalMap200ResponseDto someMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
    return this;
  }

  public FakeBigDecimalMap200ResponseDto putSomeMapItem(String key, BigDecimal someMapItem) {
    if (this.someMap == null) {
      this.someMap = new HashMap<>();
    }
    this.someMap.put(key, someMapItem);
    return this;
  }

  /**
   * Get someMap
   * @return someMap
   */
  @Valid 
  @Schema(name = "someMap", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("someMap")
  public Map<String, BigDecimal> getSomeMap() {
    return someMap;
  }

  @JsonProperty("someMap")
  public void setSomeMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FakeBigDecimalMap200ResponseDto fakeBigDecimalMap200Response = (FakeBigDecimalMap200ResponseDto) o;
    return Objects.equals(this.someId, fakeBigDecimalMap200Response.someId) &&
        Objects.equals(this.someMap, fakeBigDecimalMap200Response.someMap);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someId, someMap);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FakeBigDecimalMap200ResponseDto {\n");
    sb.append("    someId: ").append(toIndentedString(someId)).append("\n");
    sb.append("    someMap: ").append(toIndentedString(someMap)).append("\n");
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

