package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Model for testing reserved words
 */

@ApiModel(description = "Model for testing reserved words")
@JsonTypeName("Return")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class ReturnDto {

  private Integer _return;

  public ReturnDto _return(Integer _return) {
    this._return = _return;
    return this;
  }

  /**
   * Get _return
   * @return _return
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("return")
  public Integer getReturn() {
    return _return;
  }

  public void setReturn(Integer _return) {
    this._return = _return;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ReturnDto _return = (ReturnDto) o;
    return Objects.equals(this._return, _return._return);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_return);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReturnDto {\n");
    sb.append("    _return: ").append(toIndentedString(_return)).append("\n");
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
  
  public static class Builder {

    private ReturnDto instance;

    public Builder() {
      this(new ReturnDto());
    }

    protected Builder(ReturnDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ReturnDto value) { 
      this.instance.setReturn(value._return);
      return this;
    }

    public ReturnDto.Builder _return(Integer _return) {
      this.instance._return(_return);
      return this;
    }
    
    /**
    * returns a built ReturnDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ReturnDto build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static ReturnDto.Builder builder() {
    return new ReturnDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ReturnDto.Builder toBuilder() {
    ReturnDto.Builder builder = new ReturnDto.Builder();
    return builder.copyOf(this);
  }

}

