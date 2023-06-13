package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import io.helidon.webserver.Handler;
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


public abstract class AnotherFakeService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(AnotherFakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public AnotherFakeService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
        rules.patch("/another-fake/dummy", Handler.create(Client.class, this::call123testSpecialTags));
    }


    /**
     * PATCH /another-fake/dummy : To test special tags.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void call123testSpecialTags(ServerRequest request, ServerResponse response, Client client) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(client);
                
                handleCall123testSpecialTags(request, response, client);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PATCH /another-fake/dummy : To test special tags.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    abstract void handleCall123testSpecialTags(ServerRequest request, ServerResponse response, Client client);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
