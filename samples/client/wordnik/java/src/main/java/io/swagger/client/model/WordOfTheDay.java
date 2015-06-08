package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;
import io.swagger.client.model.ContentProvider;
import io.swagger.client.model.SimpleDefinition;
import io.swagger.client.model.SimpleExample;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;


@ApiModel(description = "")
public class WordOfTheDay {

    private Long id = null;
    private String parentId = null;
    private String category = null;
    private String createdBy = null;
    private Date createdAt = null;
    private ContentProvider contentProvider = null;
    private String htmlExtra = null;
    private String word = null;
    private List<SimpleDefinition> definitions = new ArrayList<SimpleDefinition>();
    private List<SimpleExample> examples = new ArrayList<SimpleExample>();
    private String note = null;
    private Date publishDate = null;


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
    @JsonProperty("parentId")
    public String getParentId() {
        return parentId;
    }

    public void setParentId(String parentId) {
        this.parentId = parentId;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("category")
    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("createdBy")
    public String getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("createdAt")
    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("contentProvider")
    public ContentProvider getContentProvider() {
        return contentProvider;
    }

    public void setContentProvider(ContentProvider contentProvider) {
        this.contentProvider = contentProvider;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("htmlExtra")
    public String getHtmlExtra() {
        return htmlExtra;
    }

    public void setHtmlExtra(String htmlExtra) {
        this.htmlExtra = htmlExtra;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("word")
    public String getWord() {
        return word;
    }

    public void setWord(String word) {
        this.word = word;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("definitions")
    public List<SimpleDefinition> getDefinitions() {
        return definitions;
    }

    public void setDefinitions(List<SimpleDefinition> definitions) {
        this.definitions = definitions;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("examples")
    public List<SimpleExample> getExamples() {
        return examples;
    }

    public void setExamples(List<SimpleExample> examples) {
        this.examples = examples;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("note")
    public String getNote() {
        return note;
    }

    public void setNote(String note) {
        this.note = note;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("publishDate")
    public Date getPublishDate() {
        return publishDate;
    }

    public void setPublishDate(Date publishDate) {
        this.publishDate = publishDate;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class WordOfTheDay {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  parentId: ").append(parentId).append("\n");
        sb.append("  category: ").append(category).append("\n");
        sb.append("  createdBy: ").append(createdBy).append("\n");
        sb.append("  createdAt: ").append(createdAt).append("\n");
        sb.append("  contentProvider: ").append(contentProvider).append("\n");
        sb.append("  htmlExtra: ").append(htmlExtra).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("  definitions: ").append(definitions).append("\n");
        sb.append("  examples: ").append(examples).append("\n");
        sb.append("  note: ").append(note).append("\n");
        sb.append("  publishDate: ").append(publishDate).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
