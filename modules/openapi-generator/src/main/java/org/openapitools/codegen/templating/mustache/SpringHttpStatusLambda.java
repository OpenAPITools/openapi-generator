package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Returns the Spring {@code org.springframework.http.HttpStatus} enumeration for the given status text.
 * It throws an IllegalArgumentException if the status text is handled by the Spring framework.
 *
 * Register:
 * <pre>
 * additionalProperties.put("springHttpStatus", new SpringHttpStatusLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#springHttpStatus}}{{statusCode}}{{/springHttpStatus}}
 * </pre>
 */
public class SpringHttpStatusLambda implements Mustache.Lambda {

    final static String HTTP_STATUS_PREFIX = "HttpStatus.";

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        final String httpCode = fragment.execute();
        switch (httpCode) {
            case "202":
                writer.write(HTTP_STATUS_PREFIX + "ACCEPTED");
                break;
            case "208":
                writer.write(HTTP_STATUS_PREFIX + "ALREADY_REPORTED");
                break;
            case "502":
                writer.write(HTTP_STATUS_PREFIX + "BAD_GATEWAY");
                break;
            case "400":
                writer.write(HTTP_STATUS_PREFIX + "BAD_REQUEST");
                break;
            case "509":
                writer.write(HTTP_STATUS_PREFIX + "BANDWIDTH_LIMIT_EXCEEDED");
                break;
            case "409":
                writer.write(HTTP_STATUS_PREFIX + "CONFLICT");
                break;
            case "100":
                writer.write(HTTP_STATUS_PREFIX + "CONTINUE");
                break;
            case "201":
                writer.write(HTTP_STATUS_PREFIX + "CREATED");
                break;
            case "103":
                writer.write(HTTP_STATUS_PREFIX + "EARLY_HINTS");
                break;
            case "417":
                writer.write(HTTP_STATUS_PREFIX + "EXPECTATION_FAILED");
                break;
            case "424":
                writer.write(HTTP_STATUS_PREFIX + "FAILED_DEPENDENCY");
                break;
            case "403":
                writer.write(HTTP_STATUS_PREFIX + "FORBIDDEN");
                break;
            case "302":
                writer.write(HTTP_STATUS_PREFIX + "FOUND");
                break;
            case "504":
                writer.write(HTTP_STATUS_PREFIX + "GATEWAY_TIMEOUT");
                break;
            case "410":
                writer.write(HTTP_STATUS_PREFIX + "GONE");
                break;
            case "505":
                writer.write(HTTP_STATUS_PREFIX + "HTTP_VERSION_NOT_SUPPORTED");
                break;
            case "418":
                writer.write(HTTP_STATUS_PREFIX + "I_AM_A_TEAPOT");
                break;
            case "226":
                writer.write(HTTP_STATUS_PREFIX + "IM_USED");
                break;
            case "507":
                writer.write(HTTP_STATUS_PREFIX + "INSUFFICIENT_STORAGE");
                break;
            case "500":
                writer.write(HTTP_STATUS_PREFIX + "INTERNAL_SERVER_ERROR");
                break;
            case "411":
                writer.write(HTTP_STATUS_PREFIX + "LENGTH_REQUIRED");
                break;
            case "423":
                writer.write(HTTP_STATUS_PREFIX + "LOCKED");
                break;
            case "508":
                writer.write(HTTP_STATUS_PREFIX + "LOOP_DETECTED");
                break;
            case "405":
                writer.write(HTTP_STATUS_PREFIX + "METHOD_NOT_ALLOWED");
                break;
            case "301":
                writer.write(HTTP_STATUS_PREFIX + "MOVED_PERMANENTLY");
                break;
            case "207":
                writer.write(HTTP_STATUS_PREFIX + "MULTI_STATUS");
                break;
            case "300":
                writer.write(HTTP_STATUS_PREFIX + "MULTIPLE_CHOICES");
                break;
            case "511":
                writer.write(HTTP_STATUS_PREFIX + "NETWORK_AUTHENTICATION_REQUIRED");
                break;
            case "204":
                writer.write(HTTP_STATUS_PREFIX + "NO_CONTENT");
                break;
            case "203":
                writer.write(HTTP_STATUS_PREFIX + "NON_AUTHORITATIVE_INFORMATION");
                break;
            case "406":
                writer.write(HTTP_STATUS_PREFIX + "NOT_ACCEPTABLE");
                break;
            case "510":
                writer.write(HTTP_STATUS_PREFIX + "NOT_EXTENDED");
                break;
            case "404":
                writer.write(HTTP_STATUS_PREFIX + "NOT_FOUND");
                break;
            case "501":
                writer.write(HTTP_STATUS_PREFIX + "NOT_IMPLEMENTED");
                break;
            case "304":
                writer.write(HTTP_STATUS_PREFIX + "NOT_MODIFIED");
                break;
            case "":
            case "200":
                writer.write(HTTP_STATUS_PREFIX + "OK");
                break;
            case "206":
                writer.write(HTTP_STATUS_PREFIX + "PARTIAL_CONTENT");
                break;
            case "413":
                writer.write(HTTP_STATUS_PREFIX + "PAYLOAD_TOO_LARGE");
                break;
            case "402":
                writer.write(HTTP_STATUS_PREFIX + "PAYMENT_REQUIRED");
                break;
            case "308":
                writer.write(HTTP_STATUS_PREFIX + "PERMANENT_REDIRECT");
                break;
            case "412":
                writer.write(HTTP_STATUS_PREFIX + "PRECONDITION_FAILED");
                break;
            case "428":
                writer.write(HTTP_STATUS_PREFIX + "PRECONDITION_REQUIRED");
                break;
            case "102":
                writer.write(HTTP_STATUS_PREFIX + "PROCESSING");
                break;
            case "407":
                writer.write(HTTP_STATUS_PREFIX + "PROXY_AUTHENTICATION_REQUIRED");
                break;
            case "431":
                writer.write(HTTP_STATUS_PREFIX + "REQUEST_HEADER_FIELDS_TOO_LARGE");
                break;
            case "408":
                writer.write(HTTP_STATUS_PREFIX + "REQUEST_TIMEOUT");
                break;
            case "416":
                writer.write(HTTP_STATUS_PREFIX + "REQUESTED_RANGE_NOT_SATISFIABLE");
                break;
            case "205":
                writer.write(HTTP_STATUS_PREFIX + "RESET_CONTENT");
                break;
            case "303":
                writer.write(HTTP_STATUS_PREFIX + "SEE_OTHER");
                break;
            case "503":
                writer.write(HTTP_STATUS_PREFIX + "SERVICE_UNAVAILABLE");
                break;
            case "101":
                writer.write(HTTP_STATUS_PREFIX + "SWITCHING_PROTOCOLS");
                break;
            case "307":
                writer.write(HTTP_STATUS_PREFIX + "TEMPORARY_REDIRECT");
                break;
            case "425":
                writer.write(HTTP_STATUS_PREFIX + "TOO_EARLY");
                break;
            case "429":
                writer.write(HTTP_STATUS_PREFIX + "TOO_MANY_REQUESTS");
                break;
            case "401":
                writer.write(HTTP_STATUS_PREFIX + "UNAUTHORIZED");
                break;
            case "451":
                writer.write(HTTP_STATUS_PREFIX + "UNAVAILABLE_FOR_LEGAL_REASONS");
                break;
            case "422":
                writer.write(HTTP_STATUS_PREFIX + "UNPROCESSABLE_ENTITY");
                break;
            case "415":
                writer.write(HTTP_STATUS_PREFIX + "UNSUPPORTED_MEDIA_TYPE");
                break;
            case "426":
                writer.write(HTTP_STATUS_PREFIX + "UPGRADE_REQUIRED");
                break;
            case "414":
                writer.write(HTTP_STATUS_PREFIX + "URI_TOO_LONG");
                break;
            case "506":
                writer.write(HTTP_STATUS_PREFIX + "VARIANT_ALSO_NEGOTIATES");
                break;
            default:
               throw new IllegalArgumentException("The given HTTP status code: " + httpCode
                       + " is not supported by the 'org.springframework.http.HttpStatus' enum.");
        }
    }
}
