package org.openapitools.server.model;

import java.math.BigDecimal;



public class NumberOnly   {

    private BigDecimal justNumber;

    /**
     * Default constructor.
     */
    public NumberOnly() {
    // JSON-B / Jackson
    }

    /**
     * Create NumberOnly.
     *
     * @param justNumber justNumber
     */
    public NumberOnly(
        BigDecimal justNumber
    ) {
        this.justNumber = justNumber;
    }



    /**
     * Get justNumber
     * @return justNumber
     */
    public BigDecimal getJustNumber() {
        return justNumber;
    }

    public void setJustNumber(BigDecimal justNumber) {
        this.justNumber = justNumber;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class NumberOnly {\n");
        
        sb.append("    justNumber: ").append(toIndentedString(justNumber)).append("\n");
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

