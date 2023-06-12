package org.openapitools.server.model;



/**
 * Model for testing model with \"_class\" property
 */
public class ClassModel   {

    private String propertyClass;

    /**
     * Default constructor.
     */
    public ClassModel() {
    // JSON-B / Jackson
    }

    /**
     * Create ClassModel.
     *
     * @param propertyClass propertyClass
     */
    public ClassModel(
        String propertyClass
    ) {
        this.propertyClass = propertyClass;
    }



    /**
     * Get propertyClass
     * @return propertyClass
     */
    public String getPropertyClass() {
        return propertyClass;
    }

    public void setPropertyClass(String propertyClass) {
        this.propertyClass = propertyClass;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ClassModel {\n");
        
        sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
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

