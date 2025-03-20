package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import java.util.Optional;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'FakeClassnameTags123'",
                             version = "stable")
public abstract class FakeClassnameTags123Service implements HttpService {


    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected TestClassnameOp testClassnameOp = createTestClassnameOp();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.patch("/", this::testClassname);
    }


    /**
     * PATCH /fake_classname_test : To test class name in snake case.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testClassname(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: Client
        Client client = testClassnameOp.client(request, validator);
        validator.require("client", client);

        validator.execute();

        handleTestClassname(request, response, 
                    client);
    }

    /**
     * Handle PATCH /fake_classname_test : To test class name in snake case.
     *
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    protected abstract void handleTestClassname(ServerRequest request, ServerResponse response, 
                Client client);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testClassname operation.
     * <p>
     *     Developers can override this method if they extend the FakeClassnameTags123Service class.
     * </p>
     *
     * @return new TestClassname
     */
    protected TestClassnameOp createTestClassnameOp() {
        return new TestClassnameOp();
    }

    /**
     * Helper elements for the {@code testClassname} operation.
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
    public static class TestClassnameOp {

        /**
         * Prepares the client parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return client parameter value
         */
        protected Client client(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Client.class)
                : null;
        }

        /**
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Client response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the testClassname operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Client response;
                @Override
                public Response200 build() {
                    return new Response200(response);
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
                Builder response(Client response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
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
    System.out.println("Service FakeClassnameTags123Service is down. Goodbye!");
    }


}
