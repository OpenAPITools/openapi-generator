package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 **/
@ApiModel(description = "Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.")
@JsonTypeName("HealthCheckResult")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class HealthCheckResult  implements Serializable {
  private String nullableMessage;

  protected HealthCheckResult(HealthCheckResultBuilder<?, ?> b) {
    this.nullableMessage = b.nullableMessage;
  }

  public HealthCheckResult() {
  }

  /**
   **/
  public HealthCheckResult nullableMessage(String nullableMessage) {
    this.nullableMessage = nullableMessage;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("NullableMessage")
  public String getNullableMessage() {
    return nullableMessage;
  }

  @JsonProperty("NullableMessage")
  public void setNullableMessage(String nullableMessage) {
    this.nullableMessage = nullableMessage;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    HealthCheckResult healthCheckResult = (HealthCheckResult) o;
    return Objects.equals(this.nullableMessage, healthCheckResult.nullableMessage);
  }

  @Override
  public int hashCode() {
    return Objects.hash(nullableMessage);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HealthCheckResult {\n");
    
    sb.append("    nullableMessage: ").append(toIndentedString(nullableMessage)).append("\n");
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


  public static HealthCheckResultBuilder<?, ?> builder() {
    return new HealthCheckResultBuilderImpl();
  }

  private static final class HealthCheckResultBuilderImpl extends HealthCheckResultBuilder<HealthCheckResult, HealthCheckResultBuilderImpl> {

    @Override
    protected HealthCheckResultBuilderImpl self() {
      return this;
    }

    @Override
    public HealthCheckResult build() {
      return new HealthCheckResult(this);
    }
  }

  public static abstract class HealthCheckResultBuilder<C extends HealthCheckResult, B extends HealthCheckResultBuilder<C, B>>  {
    private String nullableMessage;
    protected abstract B self();

    public abstract C build();

    public B nullableMessage(String nullableMessage) {
      this.nullableMessage = nullableMessage;
      return self();
    }
  }
}

