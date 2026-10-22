package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
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
 * OuterComposite
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class OuterComposite {

  @Valid 
  @Schema(name = "my_number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal myNumber;

  
  @Schema(name = "my_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String myString;

  
  @Schema(name = "my_boolean", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean myBoolean;

  public OuterComposite myNumber(@Nullable BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }


  public OuterComposite myString(@Nullable String myString) {
    this.myString = myString;
    return this;
  }


  public OuterComposite myBoolean(@Nullable Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
  }



}

