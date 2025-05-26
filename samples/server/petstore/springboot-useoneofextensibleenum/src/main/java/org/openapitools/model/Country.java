package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
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
@JsonDeserialize(using = Country.EnumDeserializer.class)
public interface Country {
    @Valid
    @JsonValue
    String getValue();

    static Country fromValue(String value) {
        try {
          return SubsetCountry.fromValue(value);
        } catch (IllegalArgumentException e) {
          // continue
        }
        return new CountryString(value);
    }

    // custom jackson deserializer
    class EnumDeserializer extends StdDeserializer<Country> {

       public EnumDeserializer() {
          super(Country.class);
       }

       @Override
       public Country deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
         String value = p.readValueAs(String.class);
         return Country.fromValue(value);
      }
    }
    /**
    * other coutries
    */
    class CountryString implements Country {
        private final String value;
        public CountryString(String value) {
            this.value = value;
        }

        @NotNull @Pattern(regexp = "^[A-Z]{2}$")  
        @Override
        public String getValue() {
            return value;
        }
        @Override
        public String toString() {
            return "CountryString:" + value;
        }

        @Override
        public final boolean equals(Object o) {
          if (!(o instanceof CountryString )) return false;
          return value.equals(((CountryString)o).value);
        }

        @Override
        public int hashCode() {
          return value.hashCode();
        }
    }
}
