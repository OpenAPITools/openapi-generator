package org.openapitools.codegen.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import io.swagger.v3.oas.models.media.Schema;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SchemaUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(SchemaUtils.class);

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    static {
        BasicPolymorphicTypeValidator ptv = BasicPolymorphicTypeValidator.builder()
                .allowIfSubType(Object.class)
                .build();
        OBJECT_MAPPER.activateDefaultTyping(ptv, ObjectMapper.DefaultTyping.EVERYTHING);
    }

    public static Schema cloneSchema(Schema schema) {
        try {
            String json = OBJECT_MAPPER.writeValueAsString(schema);
            return OBJECT_MAPPER.readValue(json, schema.getClass());
        } catch (JsonProcessingException ex) {
            LOGGER.error("Can't clone schema {}", schema, ex);
            return schema;
        }
    }
}
