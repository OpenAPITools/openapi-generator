package org.openapitools.vertxweb.server;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.reactivex.core.AbstractVerticle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MainVerticle extends AbstractVerticle {

    private static final Logger logger = LoggerFactory.getLogger(MainVerticle.class);

    @Override
    public void start(Future<Void> future) {

        DeploymentOptions options = new DeploymentOptions();
        options.setInstances(Runtime.getRuntime().availableProcessors());

        vertx.deployVerticle(HttpServerVerticle.class.getName(), options, res -> {
            if (!res.succeeded()) {
                logger.error("Deployment fail reason: ", res.cause());
            }
        });
    }
}
    

