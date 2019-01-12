package org.openapitools.api;

import java.util.List;
import org.openapitools.model.User;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface UserApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * @see UserApi#createUser
     */
    default Mono<ResponseEntity<Void>> createUser(Mono<User> body,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#createUsersWithArrayInput
     */
    default Mono<ResponseEntity<Void>> createUsersWithArrayInput(Flux<List> body,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#createUsersWithListInput
     */
    default Mono<ResponseEntity<Void>> createUsersWithListInput(Flux<List> body,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#deleteUser
     */
    default Mono<ResponseEntity<Void>> deleteUser(String username,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#getUserByName
     */
    default Mono<ResponseEntity<User>> getUserByName(String username,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                result = ApiUtil.getExampleResponse(exchange, "{  \"firstName\" : \"firstName\",  \"lastName\" : \"lastName\",  \"password\" : \"password\",  \"userStatus\" : 6,  \"phone\" : \"phone\",  \"id\" : 0,  \"email\" : \"email\",  \"username\" : \"username\"}");
                break;
            }
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                result = ApiUtil.getExampleResponse(exchange, "<User>  <id>123456789</id>  <username>aeiou</username>  <firstName>aeiou</firstName>  <lastName>aeiou</lastName>  <email>aeiou</email>  <password>aeiou</password>  <phone>aeiou</phone>  <userStatus>123</userStatus></User>");
                break;
            }
        }
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#loginUser
     */
    default Mono<ResponseEntity<String>> loginUser(String username,
        String password,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#logoutUser
     */
    default Mono<ResponseEntity<Void>> logoutUser(ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

    /**
     * @see UserApi#updateUser
     */
    default Mono<ResponseEntity<Void>> updateUser(String username,
        Mono<User> body,
        ServerWebExchange exchange) {
        Mono<Void> result = Mono.empty();
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        return result.then(Mono.empty());

    }

}
