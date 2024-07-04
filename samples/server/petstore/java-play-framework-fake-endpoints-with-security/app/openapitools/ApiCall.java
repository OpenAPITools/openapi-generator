package openapitools;

import com.google.inject.Inject;
import play.mvc.Action;
import play.mvc.Http;
import play.mvc.Result;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

public class ApiCall extends Action<OpenAPIUtils.ApiAction> {

    @Inject
    private ApiCall() {}

    public CompletionStage<Result> call(Http.Request request) {
        try {
            //TODO: Do stuff you want to handle with each API call (metrics, logging, etc..)
            return delegate.call(request);
        } catch (Throwable t) {
            //TODO: log the error in your metric

            //We rethrow this error so it will be caught in the ErrorHandler
            throw t;
        }
    }
}