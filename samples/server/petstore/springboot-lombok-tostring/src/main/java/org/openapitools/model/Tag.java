package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.lang.Nullable;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class Tag {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Long id;

  @JsonInclude(JsonInclude.Include.NON_NULL)
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

