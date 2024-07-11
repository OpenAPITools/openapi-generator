package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;


public abstract class DefaultService implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(DefaultService.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected FooGet fooGet = fooGet();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.get("/foo", this::fooGet);
    }


    /**
     * GET /foo.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fooGet(ServerRequest request, ServerResponse response) { 

                handleFooGet(request, response);
    }

    /**
     * Handle GET /foo.
     *
     * @param request the server request
     * @param response the server response
     */
    protected abstract void handleFooGet(ServerRequest request, ServerResponse response);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fooGet operation.
     * <p>
     *     Developers can override this method if they extend the DefaultService class.
     * </p>
     *
     * @return new FooGet
     */
    protected FooGet fooGet() {
        return new FooGet();
    }

    /**
     * Helper elements for the fooGet operation.
     */
    static protected class FooGet {

        /**
         * Responses for operation {@code fooGet} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Default result.
             *
             * @param status (required) Status value to be sent with this default result
             * @param response 
             */
            record Default(Status status, FooGetDefaultResponse response)     {

                /**
                 * Creates a result builder for the default result
                 * for the fooGet operation; there are no required result values for this response.
                 *
                 * @return new builder for status 0
                 */
                static Builder builder(Status status) {
                    return new Builder(status);
                }

                static class Builder implements io.helidon.common.Builder<Builder, Default> {

                    private FooGetDefaultResponse response;
                    private final Status status;

                    Builder(Status status) {
                        this.status = status;

                    }

                    @Override
                    public Default build() {
                        return new Default(status,
                                response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(FooGetDefaultResponse response) {
                        this.response = response;
                        return this;
                    }
                }

               /**
            * Constructor for a result for the default result
            * for the fooGet operation, verifying non-null values for required return data.
            *
            * @param response returned entity
            */
            public Default(Status status, FooGetDefaultResponse response) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(Result.class.getName()));
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
                this.response = response;
            }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(status);
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service DefaultService is down. Goodbye!");
    }


}
