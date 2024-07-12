package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;


public abstract class FakeClassnameTags123Service implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(FakeClassnameTags123Service.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected TestClassname testClassname = testClassname();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.patch("/fake_classname_test", this::testClassname);
    }


    /**
     * PATCH /fake_classname_test : To test class name in snake case.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testClassname(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Client
        Client client = testClassname.client(request, validator);
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
    protected TestClassname testClassname() {
        return new TestClassname();
    }

    /**
     * Helper elements for the testClassname operation.
     */
    static protected class TestClassname {

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
         * Responses for operation {@code testClassname} organized by response status.
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
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(Client response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testClassname operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                /**
                 * Sets the declared HTTP status and sends the response.
                 *
                 */
                static void send(ServerResponse serverResponse) {
                    builder().apply(serverResponse);
                }

                /**
                 * Builder for the S200 result.
                 */
                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private Client response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    /**
                     * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                     * assigning the HTTP status, any response headers, and any response entity.
                     * <p>
                     *     Equivalent to {@snippet :
                     *     build().apply(serverResponse);
                     *     }
                     * </p>
                     *
                     * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                     */
                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
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
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
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
    System.out.println("Service FakeClassnameTags123Service is down. Goodbye!");
    }


}
