package org.openapitools.server.model;

import org.openapitools.jackson.nullable.JsonNullable;


/**
 * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 */
public class HealthCheckResult   {

    private String nullableMessage;

    /**
     * Default constructor.
     */
    public HealthCheckResult() {
    // JSON-B / Jackson
    }

    /**
     * Create HealthCheckResult.
     *
     * @param nullableMessage nullableMessage
     */
    public HealthCheckResult(
        String nullableMessage
    ) {
        this.nullableMessage = nullableMessage;
    }



    /**
     * Get nullableMessage
     * @return nullableMessage
     */
    public String getNullableMessage() {
        return nullableMessage;
    }

    public void setNullableMessage(String nullableMessage) {
        this.nullableMessage = nullableMessage;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class HealthCheckResult {\n");
        
        sb.append("    nullableMessage: ").append(toIndentedString(nullableMessage)).append("\n");
        sb.append("}");
        return sb.toString();
    }

    /**
     * Convert the given object to string with each line indented by 4 spaces
     * (except the first line).
    */
    private static String toIndentedString(Object o) {
        if (o == null) {
          return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

