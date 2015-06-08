package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class ScoredWord {

    private Integer position = null;
    private Long id = null;
    private Integer docTermCount = null;
    private String lemma = null;
    private String wordType = null;
    private Float score = null;
    private Long sentenceId = null;
    private String word = null;
    private Boolean stopword = null;
    private Double baseWordScore = null;
    private String partOfSpeech = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("position")
    public Integer getPosition() {
        return position;
    }

    public void setPosition(Integer position) {
        this.position = position;
    }


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
    @JsonProperty("docTermCount")
    public Integer getDocTermCount() {
        return docTermCount;
    }

    public void setDocTermCount(Integer docTermCount) {
        this.docTermCount = docTermCount;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("lemma")
    public String getLemma() {
        return lemma;
    }

    public void setLemma(String lemma) {
        this.lemma = lemma;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("wordType")
    public String getWordType() {
        return wordType;
    }

    public void setWordType(String wordType) {
        this.wordType = wordType;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("score")
    public Float getScore() {
        return score;
    }

    public void setScore(Float score) {
        this.score = score;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("sentenceId")
    public Long getSentenceId() {
        return sentenceId;
    }

    public void setSentenceId(Long sentenceId) {
        this.sentenceId = sentenceId;
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
    @JsonProperty("stopword")
    public Boolean getStopword() {
        return stopword;
    }

    public void setStopword(Boolean stopword) {
        this.stopword = stopword;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("baseWordScore")
    public Double getBaseWordScore() {
        return baseWordScore;
    }

    public void setBaseWordScore(Double baseWordScore) {
        this.baseWordScore = baseWordScore;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("partOfSpeech")
    public String getPartOfSpeech() {
        return partOfSpeech;
    }

    public void setPartOfSpeech(String partOfSpeech) {
        this.partOfSpeech = partOfSpeech;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ScoredWord {\n");

        sb.append("  position: ").append(position).append("\n");
        sb.append("  id: ").append(id).append("\n");
        sb.append("  docTermCount: ").append(docTermCount).append("\n");
        sb.append("  lemma: ").append(lemma).append("\n");
        sb.append("  wordType: ").append(wordType).append("\n");
        sb.append("  score: ").append(score).append("\n");
        sb.append("  sentenceId: ").append(sentenceId).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("  stopword: ").append(stopword).append("\n");
        sb.append("  baseWordScore: ").append(baseWordScore).append("\n");
        sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
