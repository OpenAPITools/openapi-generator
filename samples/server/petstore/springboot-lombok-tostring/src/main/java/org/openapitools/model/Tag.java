package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A tag for a pet
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "Tag", description = "A tag for a pet")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class Tag {

  private Long id;

  private String name;

  public Tag id(Long id) {
    this.id = id;
    return this;
  }


  public Tag name(String name) {
    this.name = name;
    return this;
  }



  
  public static class Builder {

    private Tag instance;

    public Builder() {
      this(new Tag());
    }

    protected Builder(Tag instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Tag value) { 
      this.instance.setId(value.id);
      this.instance.setName(value.name);
      return this;
    }

    public Tag.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    
    public Tag.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    /**
    * returns a built Tag instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Tag build() {
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
  public static Tag.Builder builder() {
    return new Tag.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Tag.Builder toBuilder() {
    Tag.Builder builder = new Tag.Builder();
    return builder.copyOf(this);
  }

}

