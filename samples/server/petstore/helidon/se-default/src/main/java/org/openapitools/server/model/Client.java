package org.openapitools.server.model;




public class Client   {

    private String client;

    /**
     * Default constructor.
     */
    public Client() {
    // JSON-B / Jackson
    }

    /**
     * Create Client.
     *
     * @param client client
     */
    public Client(
        String client
    ) {
        this.client = client;
    }



    /**
     * Get client
     * @return client
     */
    public String getClient() {
        return client;
    }

    public void setClient(String client) {
        this.client = client;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Client {\n");
        
        sb.append("    client: ").append(toIndentedString(client)).append("\n");
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

