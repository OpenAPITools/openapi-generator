package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;

import java.util.ArrayList;
import java.util.List;


@ApiModel(description = "")
public class WordObject {

    private Long id = null;
    private String word = null;
    private String originalWord = null;
    private List<String> suggestions = new ArrayList<String>();
    private String canonicalForm = null;
    private String vulgar = null;


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
    @JsonProperty("originalWord")
    public String getOriginalWord() {
        return originalWord;
    }

    public void setOriginalWord(String originalWord) {
        this.originalWord = originalWord;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("suggestions")
    public List<String> getSuggestions() {
        return suggestions;
    }

    public void setSuggestions(List<String> suggestions) {
        this.suggestions = suggestions;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("canonicalForm")
    public String getCanonicalForm() {
        return canonicalForm;
    }

    public void setCanonicalForm(String canonicalForm) {
        this.canonicalForm = canonicalForm;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("vulgar")
    public String getVulgar() {
        return vulgar;
    }

    public void setVulgar(String vulgar) {
        this.vulgar = vulgar;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class WordObject {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("  originalWord: ").append(originalWord).append("\n");
        sb.append("  suggestions: ").append(suggestions).append("\n");
        sb.append("  canonicalForm: ").append(canonicalForm).append("\n");
        sb.append("  vulgar: ").append(vulgar).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
