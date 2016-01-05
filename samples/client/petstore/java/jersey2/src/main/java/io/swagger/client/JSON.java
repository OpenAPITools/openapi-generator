package io.swagger.client;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.datatype.joda.*;

import java.text.DateFormat;

import javax.ws.rs.ext.ContextResolver;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-01-05T14:39:17.660+08:00")
public class JSON implements ContextResolver<ObjectMapper> {
  private ObjectMapper mapper;

  public JSON() {
    mapper = new ObjectMapper();
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    mapper.registerModule(new JodaModule());
  }

  /**
   * Set the date format for JSON (de)serialization with Date properties.
   */
  public void setDateFormat(DateFormat dateFormat) {
    mapper.setDateFormat(dateFormat);
  }

  @Override
  public ObjectMapper getContext(Class<?> type) {
    return mapper;
  }
}
