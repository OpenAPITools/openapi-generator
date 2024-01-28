package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class HasOnlyReadOnly   {

    private String bar;
    private String foo;

    /**
     * Default constructor.
     */
    public HasOnlyReadOnly() {
    // JSON-B / Jackson
    }

    /**
     * Create HasOnlyReadOnly.
     *
     * @param bar bar
     * @param foo foo
     */
    public HasOnlyReadOnly(
        String bar, 
        String foo
    ) {
        this.bar = bar;
        this.foo = foo;
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
     * Get foo
     * @return foo
     */
    public String getFoo() {
        return foo;
    }

    public void setFoo(String foo) {
        this.foo = foo;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class HasOnlyReadOnly {\n");
        
        sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
        sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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

