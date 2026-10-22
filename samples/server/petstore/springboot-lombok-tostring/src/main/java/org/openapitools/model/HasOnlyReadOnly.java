package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
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
 * HasOnlyReadOnly
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@JsonTypeName("hasOnlyReadOnly")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class HasOnlyReadOnly {

  
  @Schema(name = "bar", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("bar")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String bar;

  
  @Schema(name = "foo", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("foo")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String foo;

  public HasOnlyReadOnly bar(@Nullable String bar) {
    this.bar = bar;
    return this;
  }


  public HasOnlyReadOnly foo(@Nullable String foo) {
    this.foo = foo;
    return this;
  }



}

