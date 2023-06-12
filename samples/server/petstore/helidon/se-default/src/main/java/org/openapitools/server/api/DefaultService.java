package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.common.GenericType;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;


public abstract class DefaultService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(DefaultService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public DefaultService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
        rules.get("/foo", this::fooGet);
    }


    /**
     * GET /foo.
     * @param request the server request
     * @param response the server response
     */
    void fooGet(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                
                handleFooGet(request, response);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /foo.
     * @param request the server request
     * @param response the server response
     */
    abstract void handleFooGet(ServerRequest request, ServerResponse response);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
