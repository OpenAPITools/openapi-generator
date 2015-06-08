package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.Category;
import io.swagger.client.model.Root;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class PartOfSpeech {

    private List<Root> roots = new ArrayList<Root>();
    private List<String> storageAbbr = new ArrayList<String>();
    private List<Category> allCategories = new ArrayList<Category>();


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("roots")
    public List<Root> getRoots() {
        return roots;
    }

    public void setRoots(List<Root> roots) {
        this.roots = roots;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("storageAbbr")
    public List<String> getStorageAbbr() {
        return storageAbbr;
    }

    public void setStorageAbbr(List<String> storageAbbr) {
        this.storageAbbr = storageAbbr;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("allCategories")
    public List<Category> getAllCategories() {
        return allCategories;
    }

    public void setAllCategories(List<Category> allCategories) {
        this.allCategories = allCategories;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class PartOfSpeech {\n");

        sb.append("  roots: ").append(roots).append("\n");
        sb.append("  storageAbbr: ").append(storageAbbr).append("\n");
        sb.append("  allCategories: ").append(allCategories).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
