package org.openapitools.client;

import org.threeten.bp.*;
import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import org.openapitools.jackson.nullable.JsonNullableModule;
import com.fasterxml.jackson.datatype.threetenbp.ThreeTenModule;

import java.text.DateFormat;

import javax.ws.rs.ext.ContextResolver;


public class JSON implements ContextResolver<ObjectMapper> {
  private ObjectMapper mapper;

  public JSON() {
    mapper = new ObjectMapper();
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true);
    mapper.configure(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE, true);
    mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    mapper.setDateFormat(new RFC3339DateFormat());
    ThreeTenModule module = new ThreeTenModule();
    module.addDeserializer(Instant.class, CustomInstantDeserializer.INSTANT);
    module.addDeserializer(OffsetDateTime.class, CustomInstantDeserializer.OFFSET_DATE_TIME);
    module.addDeserializer(ZonedDateTime.class, CustomInstantDeserializer.ZONED_DATE_TIME);
    mapper.registerModule(module);
    JsonNullableModule jnm = new JsonNullableModule();
    mapper.registerModule(jnm);
  }

  /**
   * Set the date format for JSON (de)serialization with Date properties.
   * @param dateFormat Date format
   */
  public void setDateFormat(DateFormat dateFormat) {
    mapper.setDateFormat(dateFormat);
  }

  @Override
  public ObjectMapper getContext(Class<?> type) {
    return mapper;
  }

  /**
   * Get the object mapper
   *
   * @return object mapper
   */
  public ObjectMapper getMapper() { return mapper; }
}
