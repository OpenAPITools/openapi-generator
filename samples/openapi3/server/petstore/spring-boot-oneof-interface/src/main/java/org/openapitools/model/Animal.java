package org.openapitools.model;

import org.openapitools.model.Cat;
import org.openapitools.model.Dog;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import jakarta.validation.constraints.*;

import java.util.*;
import jakarta.annotation.Generated;



@JsonTypeInfo(use = JsonTypeInfo.Id.DEDUCTION)
@JsonSubTypes({
    @JsonSubTypes.Type(value = Dog.class), 
    @JsonSubTypes.Type(value = Cat.class)
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface Animal {
}
