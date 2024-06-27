package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class FakeClassnameTags123ServiceImpl implements FakeClassnameTags123Service {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(FakeClassnameTags123Service.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void testClassname(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Client client = request.content().as(Client.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service FakeClassnameTags123Service is down. Goodbye!");
    }




    /**
     * Responses for operation {@code testClassname } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestClassnameResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(Client response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the testClassname operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
