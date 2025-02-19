/*
 * OpenAPI Petstore
 *
 * This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 * OpenAPI document version: 1.0.0
 * 
 *
 * AUTO-GENERATED FILE, DO NOT MODIFY!
 */
package org.openapitools.handler;

import com.networknt.server.HandlerProvider;
import io.undertow.Handlers;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.RoutingHandler;
import io.undertow.server.handlers.PathHandler;
import io.undertow.util.Methods;

/**
 * The default implementation for {@link HandlerProvider} and {@link PathHandlerInterface}.
 *
 * <p>There are two flavors of {@link HttpHandler}s to choose from, depending on your needs:</p>
 *
 * <ul>
 * <li>
 * <b>Stateless</b>: if a specific endpoint is called more than once from multiple sessions,
 * its state is not retained – a different {@link HttpHandler} is instantiated for every new
 * session. This is the default behavior.
 * </li>
 * <li>
 * <b>Stateful</b>: if a specific endpoint is called more than once from multiple sessions,
 * its state is retained properly. For example, if you want to keep a class property that counts
 * the number of requests or the last time a request was received.
 * </li>
 * </ul>
 * <p>Note: <b>Stateful</b> flavor is more performant than <b>Stateless</b>.</p>
 */
@SuppressWarnings("TooManyFunctions")
abstract public class PathHandlerProvider implements HandlerProvider, PathHandlerInterface {
    /**
     * Returns the default base path to access this server.
     */
    @javax.annotation.Nonnull
    public String getBasePath() {
        return "/v2";
    }

    /**
     * Returns a stateless {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Endpoints bound in this method do NOT start with "/v2", and
     * it's your responsibility to configure a {@link PathHandler} with a prefix path
     * by calling {@link PathHandler#addPrefixPath} like so:</p>
     *
     * <code>pathHandler.addPrefixPath("/v2", handler)</code>
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateless and won't
    * retain any state between multiple sessions.</p>
     *
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @javax.annotation.Nonnull
    @Override
    public HttpHandler getHandler() {
        return getHandler(false);
    }

    /**
     * Returns a stateless {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateless and won't
     * retain any state between multiple sessions.</p>
     *
     * @param withBasePath if true, all endpoints would start with "/v2"
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @javax.annotation.Nonnull
    public HttpHandler getHandler(final boolean withBasePath) {
        return getHandler(withBasePath ? getBasePath() : "");
    }

    /**
     * Returns a stateless {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateless and won't
     * retain any state between multiple sessions.</p>
     *
     * @param basePath base path to set for all endpoints
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @SuppressWarnings("Convert2Lambda")
    @javax.annotation.Nonnull
    public HttpHandler getHandler(final String basePath) {
        return Handlers.routing()
            .add(Methods.POST, basePath + "/pet", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    addPet().handleRequest(exchange);
                }
            })
            .add(Methods.DELETE, basePath + "/pet/{petId}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    deletePet().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/pet/findByStatus", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    findPetsByStatus().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/pet/findByTags", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    findPetsByTags().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/pet/{petId}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    getPetById().handleRequest(exchange);
                }
            })
            .add(Methods.PUT, basePath + "/pet", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    updatePet().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/pet/{petId}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    updatePetWithForm().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/pet/{petId}/uploadImage", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    uploadFile().handleRequest(exchange);
                }
            })
            .add(Methods.DELETE, basePath + "/store/order/{orderId}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    deleteOrder().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/store/inventory", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    getInventory().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/store/order/{orderId}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    getOrderById().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/store/order", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    placeOrder().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/user", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    createUser().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/user/createWithArray", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    createUsersWithArrayInput().handleRequest(exchange);
                }
            })
            .add(Methods.POST, basePath + "/user/createWithList", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    createUsersWithListInput().handleRequest(exchange);
                }
            })
            .add(Methods.DELETE, basePath + "/user/{username}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    deleteUser().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/user/{username}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    getUserByName().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/user/login", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    loginUser().handleRequest(exchange);
                }
            })
            .add(Methods.GET, basePath + "/user/logout", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    logoutUser().handleRequest(exchange);
                }
            })
            .add(Methods.PUT, basePath + "/user/{username}", new HttpHandler() {
                @Override
                public void handleRequest(HttpServerExchange exchange) throws Exception {
                    updateUser().handleRequest(exchange);
                }
            })
            ;
    }

    /**
     * Returns a stateful {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Endpoints bound in this method do NOT start with "/v2", and
     * it's your responsibility to configure a {@link PathHandler} with a prefix path
     * by calling {@link PathHandler#addPrefixPath} like so:</p>
     *
     * <code>pathHandler.addPrefixPath("/v2", handler)</code>
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateful and will
     * retain any state between multiple sessions.</p>
     *
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @javax.annotation.Nonnull
    public HttpHandler getStatefulHandler() {
        return getStatefulHandler(false);
    }

    /**
     * Returns a stateful {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateful and will
     * retain any state between multiple sessions.</p>
     *
     * @param withBasePath if true, all endpoints would start with "/v2"
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @javax.annotation.Nonnull
    public HttpHandler getStatefulHandler(final boolean withBasePath) {
        return getStatefulHandler(withBasePath ? getBasePath() : "");
    }

    /**
     * Returns a stateful {@link HttpHandler} that configures all endpoints in this server.
     *
     * <p>Note: the endpoints bound to the returned {@link HttpHandler} are stateful and will
     * retain any state between multiple sessions.</p>
     *
     * @param basePath base path to set for all endpoints
     * @return an {@link HttpHandler} of type {@link RoutingHandler}
     */
    @javax.annotation.Nonnull
    public HttpHandler getStatefulHandler(final String basePath) {
        return Handlers.routing()
            .add(Methods.POST, basePath + "/pet", addPet())
            .add(Methods.DELETE, basePath + "/pet/{petId}", deletePet())
            .add(Methods.GET, basePath + "/pet/findByStatus", findPetsByStatus())
            .add(Methods.GET, basePath + "/pet/findByTags", findPetsByTags())
            .add(Methods.GET, basePath + "/pet/{petId}", getPetById())
            .add(Methods.PUT, basePath + "/pet", updatePet())
            .add(Methods.POST, basePath + "/pet/{petId}", updatePetWithForm())
            .add(Methods.POST, basePath + "/pet/{petId}/uploadImage", uploadFile())
            .add(Methods.DELETE, basePath + "/store/order/{orderId}", deleteOrder())
            .add(Methods.GET, basePath + "/store/inventory", getInventory())
            .add(Methods.GET, basePath + "/store/order/{orderId}", getOrderById())
            .add(Methods.POST, basePath + "/store/order", placeOrder())
            .add(Methods.POST, basePath + "/user", createUser())
            .add(Methods.POST, basePath + "/user/createWithArray", createUsersWithArrayInput())
            .add(Methods.POST, basePath + "/user/createWithList", createUsersWithListInput())
            .add(Methods.DELETE, basePath + "/user/{username}", deleteUser())
            .add(Methods.GET, basePath + "/user/{username}", getUserByName())
            .add(Methods.GET, basePath + "/user/login", loginUser())
            .add(Methods.GET, basePath + "/user/logout", logoutUser())
            .add(Methods.PUT, basePath + "/user/{username}", updateUser())
            ;
    }
}
