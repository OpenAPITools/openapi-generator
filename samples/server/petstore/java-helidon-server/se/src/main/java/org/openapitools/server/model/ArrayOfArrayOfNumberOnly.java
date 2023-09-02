package org.openapitools.server.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;



public class ArrayOfArrayOfNumberOnly   {

    private List<List<BigDecimal>> arrayArrayNumber;

    /**
     * Default constructor.
     */
    public ArrayOfArrayOfNumberOnly() {
    // JSON-B / Jackson
    }

    /**
     * Create ArrayOfArrayOfNumberOnly.
     *
     * @param arrayArrayNumber arrayArrayNumber
     */
    public ArrayOfArrayOfNumberOnly(
        List<List<BigDecimal>> arrayArrayNumber
    ) {
        this.arrayArrayNumber = arrayArrayNumber;
    }



    /**
     * Get arrayArrayNumber
     * @return arrayArrayNumber
     */
    public List<List<BigDecimal>> getArrayArrayNumber() {
        return arrayArrayNumber;
    }

    public void setArrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
        this.arrayArrayNumber = arrayArrayNumber;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ArrayOfArrayOfNumberOnly {\n");
        
        sb.append("    arrayArrayNumber: ").append(toIndentedString(arrayArrayNumber)).append("\n");
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

