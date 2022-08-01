package org.openapitools.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import feign.Response;
import feign.Types;
import feign.jackson.JacksonDecoder;

import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import org.openapitools.client.model.HttpResponse;

public class JacksonResponseDecoder extends JacksonDecoder {

    public JacksonResponseDecoder(ObjectMapper mapper) {
        super(mapper);
    }

    @Override
    public Object decode(Response response, Type type) throws IOException {
    Map<String, Collection<String>> responseHeaders = Collections.unmodifiableMap(response.headers());
        //Detects if the type is an instance of the parameterized class HttpResponse
        Type responseBodyType;
        if (Types.getRawType(type).isAssignableFrom(HttpResponse.class)) {
            //The HttpResponse class has a single type parameter, the Dto class itself
            responseBodyType = ((ParameterizedType) type).getActualTypeArguments()[0];
            Object body = super.decode(response, responseBodyType);
            return new HttpResponse(responseHeaders, body, response.status());
        } else {
            //The response is not encapsulated in the HttpResponse, decode the Dto as normal
            return super.decode(response, type);
        }
    }
}