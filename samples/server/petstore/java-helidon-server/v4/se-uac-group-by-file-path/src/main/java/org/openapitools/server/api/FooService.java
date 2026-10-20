package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import java.util.Optional;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'foo'",
                             version = "stable")
public abstract class FooService implements HttpService {


    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected FooGetOp fooGetOp = createFooGetOp();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.get("/", this::fooGet);
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
     *     Developers can override this method if they extend the FooService class.
     * </p>
     *
     * @return new FooGet
     */
    protected FooGetOp createFooGetOp() {
        return new FooGetOp();
    }

    /**
     * Helper elements for the {@code fooGet} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class FooGetOp {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         * @param response 
         */
        record Default(Status status, FooGetDefaultResponse response) {

            /**
             * Creates a response builder for the default response
             * for the fooGet operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
            }

            /**
             * Builder for the Default result.
             */
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

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
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
                ValidatorUtils.Validator validator = ValidatorUtils.validator();
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
                this.response = response;
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(status);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service FooService is down. Goodbye!");
    }


}
