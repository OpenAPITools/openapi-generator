package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.annotation.Generated;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import org.hibernate.validator.constraints.*;
import org.springframework.lang.Nullable;

/**
 * A tag for a pet
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "Tag", description = "A tag for a pet")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public class Tag {

  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Long id;

  
  @Schema(name = "name", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name")
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

