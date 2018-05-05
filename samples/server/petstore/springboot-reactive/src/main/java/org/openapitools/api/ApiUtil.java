package org.openapitools.api;

import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

public class ApiUtil {
    public static Mono<Void> getExampleResponse(ServerWebExchange exchange, String example) {
        return exchange.getResponse().writeWith(Mono.just(new DefaultDataBufferFactory().wrap(example.getBytes())));
    }
}