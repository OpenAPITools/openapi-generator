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


public abstract class FakeClassnameTags123Service implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(FakeClassnameTags123Service.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public FakeClassnameTags123Service(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
        rules.patch("/fake_classname_test", Handler.create(Client.class, this::testClassname));
    }


    /**
     * PATCH /fake_classname_test : To test class name in snake case.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void testClassname(ServerRequest request, ServerResponse response, Client client) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(client);
                
                handleTestClassname(request, response, client);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PATCH /fake_classname_test : To test class name in snake case.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    abstract void handleTestClassname(ServerRequest request, ServerResponse response, Client client);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
