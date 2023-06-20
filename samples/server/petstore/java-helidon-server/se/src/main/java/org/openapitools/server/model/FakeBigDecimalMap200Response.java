package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;



public class FakeBigDecimalMap200Response   {

    private BigDecimal someId;
    private Map<String, BigDecimal> someMap = new HashMap<>();

    /**
     * Default constructor.
     */
    public FakeBigDecimalMap200Response() {
    // JSON-B / Jackson
    }

    /**
     * Create FakeBigDecimalMap200Response.
     *
     * @param someId someId
     * @param someMap someMap
     */
    public FakeBigDecimalMap200Response(
        BigDecimal someId, 
        Map<String, BigDecimal> someMap
    ) {
        this.someId = someId;
        this.someMap = someMap;
    }



    /**
     * Get someId
     * @return someId
     */
    public BigDecimal getSomeId() {
        return someId;
    }

    public void setSomeId(BigDecimal someId) {
        this.someId = someId;
    }

    /**
     * Get someMap
     * @return someMap
     */
    public Map<String, BigDecimal> getSomeMap() {
        return someMap;
    }

    public void setSomeMap(Map<String, BigDecimal> someMap) {
        this.someMap = someMap;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FakeBigDecimalMap200Response {\n");
        
        sb.append("    someId: ").append(toIndentedString(someId)).append("\n");
        sb.append("    someMap: ").append(toIndentedString(someMap)).append("\n");
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

