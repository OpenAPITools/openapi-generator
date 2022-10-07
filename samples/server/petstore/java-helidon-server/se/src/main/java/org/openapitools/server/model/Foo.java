package org.openapitools.server.model;




public class Foo   {

    private String bar = "bar";

    /**
     * Default constructor.
     */
    public Foo() {
    // JSON-B / Jackson
    }

    /**
     * Create Foo.
     *
     * @param bar bar
     */
    public Foo(
        String bar
    ) {
        this.bar = bar;
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
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Foo {\n");
        
        sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
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

