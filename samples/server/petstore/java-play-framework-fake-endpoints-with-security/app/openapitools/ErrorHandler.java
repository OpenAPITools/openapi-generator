package openapitools;


import com.typesafe.config.Config;
import play.*;
import play.api.OptionalSourceMapper;
import play.api.UsefulException;
import play.api.routing.Router;
import play.http.DefaultHttpErrorHandler;
import play.mvc.Http.*;
import play.mvc.*;

import javax.inject.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import static play.mvc.Results.*;

@Singleton
public class ErrorHandler extends DefaultHttpErrorHandler {

    @Inject
    public ErrorHandler(Config configuration, Environment environment, OptionalSourceMapper sourceMapper, Provider<Router> routes) {
        super(configuration, environment, sourceMapper, routes);
    }

    @Override
    protected CompletionStage<Result> onDevServerError(RequestHeader request, UsefulException exception) {
        return CompletableFuture.completedFuture(
            handleExceptions(exception)
        );
    }

    @Override
    protected CompletionStage<Result> onProdServerError(RequestHeader request, UsefulException exception) {
        return CompletableFuture.completedFuture(
            handleExceptions(exception)
        );
    }

    @Override
    protected void logServerError(RequestHeader request, UsefulException usefulException) {
        //Since the error is already handled, we don't want to print anything on the console
        //But if you want to have the error printed in the console, just delete this override
    }

    private Result handleExceptions(Throwable t) {
        //TODO: Handle exception that need special response (return a special apimodel, notFound(), etc..)
        return ok();
    }
}
