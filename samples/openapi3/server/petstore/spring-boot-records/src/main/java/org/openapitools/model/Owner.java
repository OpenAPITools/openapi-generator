package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Owner
 *
 * @param id Get id
 * @param email Get email
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Owner(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "id", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("id")
    Long id,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @jakarta.validation.constraints.Email 
    @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("email")
    @Nullable String email
) {
}

