package io.swagger.model;

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
@Canonical
class User {

    Long id = null

    String username = null

    String firstName = null

    String lastName = null

    String email = null

    String password = null

    String phone = null

  /* User Status */
  Integer userStatus = null
  

}

