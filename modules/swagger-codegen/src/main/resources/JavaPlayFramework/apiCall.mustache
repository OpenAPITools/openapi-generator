package swagger;

import com.google.inject.Inject;
import play.mvc.Action;
import play.mvc.Http;
import play.mvc.Result;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

public class ApiCall extends Action<SwaggerUtils.ApiAction> {

    @Inject
    private ApiCall() {}

    public CompletionStage <Result> call(Http.Context ctx) {
        try {
            //TODO: Do stuff you want to handle with each API call (metrics, logging, etc..)
            return delegate.call(ctx);
        } catch (Throwable t) {
            //TODO: handle error the way you want
            return CompletableFuture.completedFuture(handleExceptions(t));
        }
    }

    private Result handleExceptions(Throwable t) {
        //TODO: Handle exception that need special response (return a special apimodel, etc..)
        return ok();
    }
}