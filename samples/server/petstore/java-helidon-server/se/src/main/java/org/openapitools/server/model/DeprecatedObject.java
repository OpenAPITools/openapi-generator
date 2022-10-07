package org.openapitools.server.model;




public class DeprecatedObject   {

    private String name;

    /**
     * Default constructor.
     */
    public DeprecatedObject() {
    // JSON-B / Jackson
    }

    /**
     * Create DeprecatedObject.
     *
     * @param name name
     */
    public DeprecatedObject(
        String name
    ) {
        this.name = name;
    }



    /**
     * Get name
     * @return name
     */
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class DeprecatedObject {\n");
        
        sb.append("    name: ").append(toIndentedString(name)).append("\n");
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

