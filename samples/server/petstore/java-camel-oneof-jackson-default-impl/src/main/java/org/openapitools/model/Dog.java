package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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

/**
 * Dog
 */

@JacksonXmlRootElement(localName = "Dog")
@XmlRootElement(name = "Dog")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.JavaCamelServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class Dog implements Animal {

  private Boolean bark;

  public Dog bark(Boolean bark) {
    this.bark = bark;
    return this;
  }

  /**
   * Get bark
   * @return bark
   */
  
  @Schema(name = "bark", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("bark")
  @JacksonXmlProperty(localName = "bark")
  public Boolean getBark() {
    return bark;
  }

  public void setBark(Boolean bark) {
    this.bark = bark;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Dog dog = (Dog) o;
    return Objects.equals(this.bark, dog.bark);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bark);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    bark: ").append(toIndentedString(bark)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

