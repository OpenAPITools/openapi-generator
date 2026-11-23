package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import javax.annotation.Generated;
import javax.validation.Valid;
import javax.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import org.openapitools.model.Cat;
import org.openapitools.model.Dog;



@JsonTypeInfo(use = JsonTypeInfo.Id.DEDUCTION, defaultImpl = Dog.class)
@JsonSubTypes({
    @JsonSubTypes.Type(value = Dog.class), 
    @JsonSubTypes.Type(value = Cat.class)
})
@Generated(value = "org.openapitools.codegen.languages.JavaCamelServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public interface Animal {
}
