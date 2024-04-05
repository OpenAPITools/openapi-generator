package org.openapitools.api;

import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.ext.ContextResolver;
import jakarta.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

@Provider
@Produces(MediaType.APPLICATION_JSON)
public class JacksonConfig implements ContextResolver<ObjectMapper> {

    private static final Logger LOG = LoggerFactory.getLogger(JacksonConfig.class);

    private ObjectMapper objectMapper;

    public JacksonConfig() throws Exception {
        this.objectMapper = new ObjectMapper();

        this.objectMapper.registerModule(new JavaTimeModule());

        // sample to convert any DateTime to readable timestamps
        //this.objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, true);
    }

    public ObjectMapper getContext(Class<?> objectType) {
        return objectMapper;
    }
}
