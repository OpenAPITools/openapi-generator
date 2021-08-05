package org.openapitools.api;

import java.nio.charset.StandardCharsets;
import org.springframework.core.io.buffer.DefaultDataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

public class ApiUtil {
    public static Mono<Void> getExampleResponse(ServerWebExchange exchange, String contentType, String example) {
        DefaultDataBuffer data = new DefaultDataBufferFactory().wrap(example.getBytes(StandardCharsets.UTF_8));
        ServerHttpResponse response = exchange.getResponse();
        response.getHeaders().setContentType(MediaType.valueOf(contentType));
        return response.writeWith(Mono.just(data));
    }
}
