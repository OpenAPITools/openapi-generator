package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;

public class DefaultServiceImpl implements DefaultService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(DefaultService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void fooGet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service DefaultService is down. Goodbye!");
    }




    /**
     * Responses for operation {@code fooGet } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FooGetResult {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         * @param response 
         */
        record Default(Status status,FooGetDefaultResponse response) {

            /**
             * Factory method creating a result for the default result
             * for the fooGet operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 0
             */
            static Default create(Status status) {
                return new Default(statusnull);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
