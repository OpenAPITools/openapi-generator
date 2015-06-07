package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.Category;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class Root {

    private Long id = null;
    private String name = null;
    private List<Category> categories = new ArrayList<Category>();


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("id")
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("name")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("categories")
    public List<Category> getCategories() {
        return categories;
    }

    public void setCategories(List<Category> categories) {
        this.categories = categories;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Root {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  name: ").append(name).append("\n");
        sb.append("  categories: ").append(categories).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
