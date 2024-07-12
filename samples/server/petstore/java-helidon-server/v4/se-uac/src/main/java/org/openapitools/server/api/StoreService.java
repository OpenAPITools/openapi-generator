package org.openapitools.server.api;

import java.util.HexFormat;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import io.helidon.http.Status;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'Store'",
                             version = "stable")
public abstract class StoreService implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(StoreService.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected DeleteOrder deleteOrder = deleteOrder();
    protected GetInventory getInventory = getInventory();
    protected GetOrderById getOrderById = getOrderById();
    protected PlaceOrder placeOrder = placeOrder();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.delete("/store/order/{order_id}", this::deleteOrder);
        rules.get("/store/inventory", this::getInventory);
        rules.get("/store/order/{order_id}", this::getOrderById);
        rules.post("/store/order", this::placeOrder);
    }


    /**
     * DELETE /store/order/{order_id} : Delete purchase order by ID.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void deleteOrder(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: order_id
        String orderId = deleteOrder.orderId(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: order_id
        Long orderId = getOrderById.orderId(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Order
        Order order = placeOrder.order(request, validator);
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
    protected DeleteOrder deleteOrder() {
        return new DeleteOrder();
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
    public static class DeleteOrder {

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
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the deleteOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 404}.
         */
        record result404() {

            /**
             * Creates a result builder for the status {@code 404} result
             * for the deleteOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result404> {


                @Override
                public result404 build() {
                    return new result404();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
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
    protected GetInventory getInventory() {
        return new GetInventory();
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
    public static class GetInventory {

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(Map<String, Integer> response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the getInventory operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private Map<String, Integer> response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
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
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
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
    protected GetOrderById getOrderById() {
        return new GetOrderById();
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
    public static class GetOrderById {

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
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(Order response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private Order response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
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
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 404}.
         */
        record result404() {

            /**
             * Creates a result builder for the status {@code 404} result
             * for the getOrderById operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result404> {


                @Override
                public result404 build() {
                    return new result404();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
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
    protected PlaceOrder placeOrder() {
        return new PlaceOrder();
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
    public static class PlaceOrder {

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
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(Order response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the placeOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private Order response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
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
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the placeOrder operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service StoreService is down. Goodbye!");
    }


}
