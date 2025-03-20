package org.openapitools.server.api;

import java.util.HexFormat;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import io.helidon.http.Status;

import java.util.Optional;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'store'",
                             version = "stable")
public abstract class StoreService implements HttpService {


    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected DeleteOrderOp deleteOrderOp = createDeleteOrderOp();
    protected GetInventoryOp getInventoryOp = createGetInventoryOp();
    protected GetOrderByIdOp getOrderByIdOp = createGetOrderByIdOp();
    protected PlaceOrderOp placeOrderOp = createPlaceOrderOp();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.delete("/order/{order_id}", this::deleteOrder);
        rules.get("/inventory", this::getInventory);
        rules.get("/order/{order_id}", this::getOrderById);
        rules.post("/order", this::placeOrder);
    }


    /**
     * DELETE /store/order/{order_id} : Delete purchase order by ID.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void deleteOrder(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: order_id
        String orderId = deleteOrderOp.orderId(request, validator);

        validator.require("orderId", orderId);
        validator.execute();

        handleDeleteOrder(request, response, 
                    orderId);
    }

    /**
     * Handle DELETE /store/order/{order_id} : Delete purchase order by ID.
     *
     * @param request the server request
     * @param response the server response
     * @param orderId ID of the order that needs to be deleted 
     */
    protected abstract void handleDeleteOrder(ServerRequest request, ServerResponse response, 
                String orderId);

    /**
     * GET /store/inventory : Returns pet inventories by status.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void getInventory(ServerRequest request, ServerResponse response) { 

                handleGetInventory(request, response);
    }

    /**
     * Handle GET /store/inventory : Returns pet inventories by status.
     *
     * @param request the server request
     * @param response the server response
     */
    protected abstract void handleGetInventory(ServerRequest request, ServerResponse response);

    /**
     * GET /store/order/{order_id} : Find purchase order by ID.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void getOrderById(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: order_id
        Long orderId = getOrderByIdOp.orderId(request, validator);

        validator.require("orderId", orderId);
        validator.validateMin("orderId", orderId.intValue(), 1, true);
        validator.validateMax("orderId", orderId.intValue(), 5, true);
        validator.execute();

        handleGetOrderById(request, response, 
                    orderId);
    }

    /**
     * Handle GET /store/order/{order_id} : Find purchase order by ID.
     *
     * @param request the server request
     * @param response the server response
     * @param orderId ID of pet that needs to be fetched 
     */
    protected abstract void handleGetOrderById(ServerRequest request, ServerResponse response, 
                Long orderId);

    /**
     * POST /store/order : Place an order for a pet.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void placeOrder(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: Order
        Order order = placeOrderOp.order(request, validator);
        validator.require("order", order);

        validator.execute();

        handlePlaceOrder(request, response, 
                    order);
    }

    /**
     * Handle POST /store/order : Place an order for a pet.
     *
     * @param request the server request
     * @param response the server response
     * @param order order placed for purchasing the pet 
     */
    protected abstract void handlePlaceOrder(ServerRequest request, ServerResponse response, 
                Order order);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the deleteOrder operation.
     * <p>
     *     Developers can override this method if they extend the StoreService class.
     * </p>
     *
     * @return new DeleteOrder
     */
    protected DeleteOrderOp createDeleteOrderOp() {
        return new DeleteOrderOp();
    }

    /**
     * Helper elements for the {@code deleteOrder} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class DeleteOrderOp {

        /**
         * Prepares the orderId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return orderId parameter value
         */
        protected String orderId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("order_id")
                .asOptional()
                .orElse(null);
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the deleteOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 404}.
         */
        record Response404() {

            /**
             * Creates a response builder for the status {@code 404} response
             * for the deleteOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response404> {

                @Override
                public Response404 build() {
                    return new Response404();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.NOT_FOUND_404);
                _serverResponse.send();
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the getInventory operation.
     * <p>
     *     Developers can override this method if they extend the StoreService class.
     * </p>
     *
     * @return new GetInventory
     */
    protected GetInventoryOp createGetInventoryOp() {
        return new GetInventoryOp();
    }

    /**
     * Helper elements for the {@code getInventory} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class GetInventoryOp {

        /**
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Map<String, Integer> response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the getInventory operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Map<String, Integer> response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(Map<String, Integer> response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the getOrderById operation.
     * <p>
     *     Developers can override this method if they extend the StoreService class.
     * </p>
     *
     * @return new GetOrderById
     */
    protected GetOrderByIdOp createGetOrderByIdOp() {
        return new GetOrderByIdOp();
    }

    /**
     * Helper elements for the {@code getOrderById} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class GetOrderByIdOp {

        /**
         * Prepares the orderId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return orderId parameter value
         */
        protected Long orderId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("order_id")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Order response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Order response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(Order response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 404}.
         */
        record Response404() {

            /**
             * Creates a response builder for the status {@code 404} response
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response404> {

                @Override
                public Response404 build() {
                    return new Response404();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.NOT_FOUND_404);
                _serverResponse.send();
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the placeOrder operation.
     * <p>
     *     Developers can override this method if they extend the StoreService class.
     * </p>
     *
     * @return new PlaceOrder
     */
    protected PlaceOrderOp createPlaceOrderOp() {
        return new PlaceOrderOp();
    }

    /**
     * Helper elements for the {@code placeOrder} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class PlaceOrderOp {

        /**
         * Prepares the order parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return order parameter value
         */
        protected Order order(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Order.class)
                : null;
        }

        /**
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Order response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the placeOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Order response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(Order response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the placeOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service StoreService is down. Goodbye!");
    }


}
