package org.openapitools.vertxweb.server;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.openapi.RouterFactory;
import io.vertx.ext.web.openapi.RouterFactoryOptions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openapitools.vertxweb.server.api.PetApiHandler;
import org.openapitools.vertxweb.server.api.StoreApiHandler;
import org.openapitools.vertxweb.server.api.UserApiHandler;

public class HttpServerVerticle extends AbstractVerticle {

    private static final Logger logger = LoggerFactory.getLogger(HttpServerVerticle.class);
    private static final String specFile = "src/main/resources/openapi.yaml";

    
    private final PetApiHandler petHandler = new PetApiHandler();
    private final StoreApiHandler storeHandler = new StoreApiHandler();
    private final UserApiHandler userHandler = new UserApiHandler();

    @Override
    public void start(Promise<Void> startPromise) {
        RouterFactory.create(vertx, specFile)
            .map(factory -> {
              factory.setOptions(new RouterFactoryOptions()
                  // For production use case, you need to enable this flag and provide the proper security handler
                  .setRequireSecurityHandlers(false)
              );
              
              petHandler.mount(factory);
              storeHandler.mount(factory);
              userHandler.mount(factory);

              Router router = factory.createRouter();
              router.errorHandler(400, this::validationFailureHandler);

              return router;
            })
            .compose(router ->
                vertx.createHttpServer()
                    .requestHandler(router)
                    .listen(8080)
            )
            .onSuccess(server -> logger.info("Http verticle deploy successful"))
            .onFailure(t -> logger.error("Http verticle failed to deploy", t))
            // Complete the start promise
            .<Void>mapEmpty().onComplete(startPromise);
    }

    private void validationFailureHandler(RoutingContext rc) {
         rc.response().setStatusCode(400)
                 .end("Bad Request : " + rc.failure().getMessage());
    }
}
