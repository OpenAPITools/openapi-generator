package org.openapitools.server;

import org.openapitools.server.api.AnotherFakeServiceImpl;
import org.openapitools.server.api.DefaultServiceImpl;
import org.openapitools.server.api.FakeServiceImpl;
import org.openapitools.server.api.FakeClassnameTags123ServiceImpl;
import org.openapitools.server.api.PetServiceImpl;
import org.openapitools.server.api.StoreServiceImpl;
import org.openapitools.server.api.UserServiceImpl;

import io.helidon.logging.common.LogConfig;
import io.helidon.config.Config;
import io.helidon.webserver.http.HttpRouting;
import io.helidon.webserver.WebServer;

/**
* The application main class.
*/
public final class Main {

    /**
    * Cannot be instantiated.
    */
    private Main() {
    }

    /**
    * Application main entry point.
    * @param args command line arguments.
    */
    public static void main(final String[] args) {
        startServer();
    }

    /**
    * Start the server.
    * @return the created {@link WebServer} instance
    */
    static WebServer startServer() {

        // load logging configuration
        LogConfig.configureRuntime();

        // By default this will pick up application.yaml from the classpath
        Config config = Config.create();
        Config.global(config);

        WebServer webserver = WebServer.builder()
                .config(config.get("server"))
                .routing(Main::routing)
                .build()
                .start();

        System.out.println("WEB server is up! http://petstore.swagger.io:80/v2");

        return webserver;
    }

    /**
     * Updates HTTP routing and implicitly registers observe providers.
     */
    static void routing(HttpRouting.Builder routing) {
        routing
            .register("/another-fake/dummy", new AnotherFakeServiceImpl())
            .register("/foo", new DefaultServiceImpl())
            .register("/fake", new FakeServiceImpl())
            .register("/fake_classname_test", new FakeClassnameTags123ServiceImpl())
            .register("/", new PetServiceImpl())/* TODO - fix path or operation grouping for better performance */
            .register("/store", new StoreServiceImpl())
            .register("/user", new UserServiceImpl());
    }
}
