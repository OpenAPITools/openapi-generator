package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.OtherCountryEnum;
import org.openapitools.model.SubsetCountry;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.core.JsonParser;
import java.io.IOException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
@JsonDeserialize(using = CountryOrOther.EnumDeserializer.class)
public interface CountryOrOther {
    @Valid
    @JsonValue
    String getValue();

    static CountryOrOther fromValue(String value) {
        try {
          return SubsetCountry.fromValue(value);
        } catch (IllegalArgumentException e) {
          // continue
        }
        try {
          return OtherCountryEnum.fromValue(value);
        } catch (IllegalArgumentException e) {
          // continue
        }
        throw new IllegalArgumentException(value + " not supported in classes " + Arrays.asList("SubsetCountry", "OtherCountryEnum"));
    }

    // custom jackson deserializer
    class EnumDeserializer extends StdDeserializer<CountryOrOther> {

       public EnumDeserializer() {
          super(CountryOrOther.class);
       }

       @Override
       public CountryOrOther deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
         String value = p.readValueAs(String.class);
         return CountryOrOther.fromValue(value);
      }
    }
}
