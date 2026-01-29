package org.openapitools.model;

import java.net.URI;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import org.springframework.lang.Nullable;

import org.openapitools.jackson.nullable.JsonNullable;

import java.time.OffsetDateTime;

import javax.validation.Valid;
import javax.validation.constraints.*;

import java.util.*;

import javax.annotation.Generated;

interface WithDefaultMethod {

    default String greet(String name) {
        return "Hello, " + name + "!";
    }
}

