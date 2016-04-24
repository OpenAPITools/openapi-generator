package io.swagger.model;

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Category;
import io.swagger.model.Tag;
import java.util.ArrayList;
import java.util.List;
@Canonical
class Pet {

    Long id = null

    Category category = null

    String name = null

    List<String> photoUrls = new ArrayList<String>()

    List<Tag> tags = new ArrayList<Tag>()

  /* pet status in the store */
  String status = null
  

}

