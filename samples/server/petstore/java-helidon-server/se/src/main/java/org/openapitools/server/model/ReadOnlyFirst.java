package org.openapitools.server.model;




public class ReadOnlyFirst   {

    private String bar;
    private String baz;

    /**
     * Default constructor.
     */
    public ReadOnlyFirst() {
    // JSON-B / Jackson
    }

    /**
     * Create ReadOnlyFirst.
     *
     * @param bar bar
     * @param baz baz
     */
    public ReadOnlyFirst(
        String bar, 
        String baz
    ) {
        this.bar = bar;
        this.baz = baz;
    }



    /**
     * Get bar
     * @return bar
     */
    public String getBar() {
        return bar;
    }

    public void setBar(String bar) {
        this.bar = bar;
    }

    /**
     * Get baz
     * @return baz
     */
    public String getBaz() {
        return baz;
    }

    public void setBaz(String baz) {
        this.baz = baz;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ReadOnlyFirst {\n");
        
        sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
        sb.append("    baz: ").append(toIndentedString(baz)).append("\n");
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

