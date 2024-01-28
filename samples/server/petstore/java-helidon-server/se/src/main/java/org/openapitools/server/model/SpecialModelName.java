package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class SpecialModelName   {

    private Long $specialPropertyName;

    /**
     * Default constructor.
     */
    public SpecialModelName() {
    // JSON-B / Jackson
    }

    /**
     * Create SpecialModelName.
     *
     * @param $specialPropertyName $specialPropertyName
     */
    public SpecialModelName(
        Long $specialPropertyName
    ) {
        this.$specialPropertyName = $specialPropertyName;
    }



    /**
     * Get $specialPropertyName
     * @return $specialPropertyName
     */
    public Long get$SpecialPropertyName() {
        return $specialPropertyName;
    }

    public void set$SpecialPropertyName(Long $specialPropertyName) {
        this.$specialPropertyName = $specialPropertyName;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class SpecialModelName {\n");
        
        sb.append("    $specialPropertyName: ").append(toIndentedString($specialPropertyName)).append("\n");
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

