package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.model.Cat;
import org.openapitools.model.Dog;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import io.swagger.v3.oas.annotations.media.Schema;

import javax.xml.bind.annotation.*;

import java.util.*;
import javax.annotation.Generated;


@JacksonXmlRootElement(localName = "Animal")
@XmlRootElement(name = "Animal")
@XmlAccessorType(XmlAccessType.FIELD)

@JsonTypeInfo(use = JsonTypeInfo.Id.DEDUCTION, defaultImpl = Dog.class)
@JsonSubTypes({
    @JsonSubTypes.Type(value = Dog.class), 
    @JsonSubTypes.Type(value = Cat.class)
})
@Generated(value = "org.openapitools.codegen.languages.JavaCamelServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public interface Animal {
}
