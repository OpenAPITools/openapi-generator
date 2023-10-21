package org.openapitools.server.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.server.model.DeprecatedObject;



public class ObjectWithDeprecatedFields   {

    private String _uuid;
    private BigDecimal id;
    private DeprecatedObject deprecatedRef;
    private List<String> bars;

    /**
     * Default constructor.
     */
    public ObjectWithDeprecatedFields() {
    // JSON-B / Jackson
    }

    /**
     * Create ObjectWithDeprecatedFields.
     *
     * @param _uuid _uuid
     * @param id id
     * @param deprecatedRef deprecatedRef
     * @param bars bars
     */
    public ObjectWithDeprecatedFields(
        String _uuid, 
        BigDecimal id, 
        DeprecatedObject deprecatedRef, 
        List<String> bars
    ) {
        this._uuid = _uuid;
        this.id = id;
        this.deprecatedRef = deprecatedRef;
        this.bars = bars;
    }



    /**
     * Get _uuid
     * @return _uuid
     */
    public String getUuid() {
        return _uuid;
    }

    public void setUuid(String _uuid) {
        this._uuid = _uuid;
    }

    /**
     * Get id
     * @return id
     */
    public BigDecimal getId() {
        return id;
    }

    public void setId(BigDecimal id) {
        this.id = id;
    }

    /**
     * Get deprecatedRef
     * @return deprecatedRef
     */
    public DeprecatedObject getDeprecatedRef() {
        return deprecatedRef;
    }

    public void setDeprecatedRef(DeprecatedObject deprecatedRef) {
        this.deprecatedRef = deprecatedRef;
    }

    /**
     * Get bars
     * @return bars
     */
    public List<String> getBars() {
        return bars;
    }

    public void setBars(List<String> bars) {
        this.bars = bars;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ObjectWithDeprecatedFields {\n");
        
        sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
        sb.append("    id: ").append(toIndentedString(id)).append("\n");
        sb.append("    deprecatedRef: ").append(toIndentedString(deprecatedRef)).append("\n");
        sb.append("    bars: ").append(toIndentedString(bars)).append("\n");
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

