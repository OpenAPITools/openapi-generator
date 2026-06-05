package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.springframework.lang.Nullable;
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
@JsonPropertyOrder({
    Tag.JSON_PROPERTY_ID,
    Tag.JSON_PROPERTY_NAME
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class Tag {

    public static final String JSON_PROPERTY_ID = "id";
  private @Nullable Long id;

    public static final String JSON_PROPERTY_NAME = "name";
  private @Nullable String name;

  public Tag id(@Nullable Long id) {
    this.id = id;
    return this;
  }


  public Tag name(@Nullable String name) {
    this.name = name;
    return this;
  }



}

