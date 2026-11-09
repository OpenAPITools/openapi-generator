package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.annotation.Generated;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import org.openapitools.model.Cat;
import org.openapitools.model.Dog;
import org.springframework.lang.Nullable;


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public sealed interface Animal permits Dog, Cat {
}
