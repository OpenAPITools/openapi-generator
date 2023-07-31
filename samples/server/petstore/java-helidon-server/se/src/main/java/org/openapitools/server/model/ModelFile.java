package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;


/**
 * Must be named `File` for test.
 */
public class ModelFile   {

    private String sourceURI;

    /**
     * Default constructor.
     */
    public ModelFile() {
    // JSON-B / Jackson
    }

    /**
     * Create ModelFile.
     *
     * @param sourceURI Test capitalization
     */
    public ModelFile(
        String sourceURI
    ) {
        this.sourceURI = sourceURI;
    }



    /**
     * Test capitalization
     * @return sourceURI
     */
    public String getSourceURI() {
        return sourceURI;
    }

    public void setSourceURI(String sourceURI) {
        this.sourceURI = sourceURI;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ModelFile {\n");
        
        sb.append("    sourceURI: ").append(toIndentedString(sourceURI)).append("\n");
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

