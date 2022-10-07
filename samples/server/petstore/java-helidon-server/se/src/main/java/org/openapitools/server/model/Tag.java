package org.openapitools.server.model;




public class Tag   {

    private Long id;
    private String name;

    /**
     * Default constructor.
     */
    public Tag() {
    // JSON-B / Jackson
    }

    /**
     * Create Tag.
     *
     * @param id id
     * @param name name
     */
    public Tag(
        Long id, 
        String name
    ) {
        this.id = id;
        this.name = name;
    }



    /**
     * Get id
     * @return id
     */
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    /**
     * Get name
     * @return name
     */
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Tag {\n");
        
        sb.append("    id: ").append(toIndentedString(id)).append("\n");
        sb.append("    name: ").append(toIndentedString(name)).append("\n");
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

