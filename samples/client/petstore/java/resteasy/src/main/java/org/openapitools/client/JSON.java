package org.openapitools.client;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import org.openapitools.jackson.nullable.JsonNullableModule;
import com.fasterxml.jackson.datatype.jsr310.*;

import java.text.DateFormat;

import javax.ws.rs.ext.ContextResolver;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class JSON implements ContextResolver<ObjectMapper> {
  private ObjectMapper mapper;

  public JSON() {
    mapper = new ObjectMapper();
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    mapper.configure(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE, false);
    mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    mapper.setDateFormat(new RFC3339DateFormat());
    JsonNullableModule jnm = new JsonNullableModule();
    mapper.registerModule(jnm);
    mapper.registerModule(new JavaTimeModule());
  }

  /**
   * Set the date format for JSON (de)serialization with Date properties.
   * @param dateFormat the date format to set
   */
  public void setDateFormat(DateFormat dateFormat) {
    mapper.setDateFormat(dateFormat);
  }

  @Override
  public ObjectMapper getContext(Class<?> type) {
    return mapper;
  }
}
