package org.openapitools.server.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;



public class ArrayOfNumberOnly   {

    private List<BigDecimal> arrayNumber = new ArrayList<>();

    /**
     * Default constructor.
     */
    public ArrayOfNumberOnly() {
    // JSON-B / Jackson
    }

    /**
     * Create ArrayOfNumberOnly.
     *
     * @param arrayNumber arrayNumber
     */
    public ArrayOfNumberOnly(
        List<BigDecimal> arrayNumber
    ) {
        this.arrayNumber = arrayNumber;
    }



    /**
     * Get arrayNumber
     * @return arrayNumber
     */
    public List<BigDecimal> getArrayNumber() {
        return arrayNumber;
    }

    public void setArrayNumber(List<BigDecimal> arrayNumber) {
        this.arrayNumber = arrayNumber;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ArrayOfNumberOnly {\n");
        
        sb.append("    arrayNumber: ").append(toIndentedString(arrayNumber)).append("\n");
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

