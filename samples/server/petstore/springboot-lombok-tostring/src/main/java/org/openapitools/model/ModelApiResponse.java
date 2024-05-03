package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Describes the result of uploading an image resource
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "ApiResponse", description = "Describes the result of uploading an image resource")
@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class ModelApiResponse {

  private Integer code;

  private String type;

  private String message;

  public ModelApiResponse() {
    super();
  }

  public ModelApiResponse code(Integer code) {
    this.code = code;
    return this;
  }


  public ModelApiResponse type(String type) {
    this.type = type;
    return this;
  }


  public ModelApiResponse message(String message) {
    this.message = message;
    return this;
  }



  
  public static class Builder {

    private ModelApiResponse instance;

    public Builder() {
      this(new ModelApiResponse());
    }

    protected Builder(ModelApiResponse instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ModelApiResponse value) { 
      this.instance.setCode(value.code);
      this.instance.setType(value.type);
      this.instance.setMessage(value.message);
      return this;
    }

    public ModelApiResponse.Builder code(Integer code) {
      this.instance.code(code);
      return this;
    }
    
    public ModelApiResponse.Builder type(String type) {
      this.instance.type(type);
      return this;
    }
    
    public ModelApiResponse.Builder message(String message) {
      this.instance.message(message);
      return this;
    }
    
    /**
    * returns a built ModelApiResponse instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ModelApiResponse build() {
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
  public static ModelApiResponse.Builder builder() {
    return new ModelApiResponse.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ModelApiResponse.Builder toBuilder() {
    ModelApiResponse.Builder builder = new ModelApiResponse.Builder();
    return builder.copyOf(this);
  }

}

