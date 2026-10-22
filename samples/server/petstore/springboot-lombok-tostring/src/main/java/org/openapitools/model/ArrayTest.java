package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.ReadOnlyFirst;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ArrayTest
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class ArrayTest {

  
  @Schema(name = "array_of_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("array_of_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<String> arrayOfString = new ArrayList<>();

  
  @Schema(name = "array_array_of_integer", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("array_array_of_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<List<Long>> arrayArrayOfInteger = new ArrayList<>();

  
  @Schema(name = "array_array_of_model", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("array_array_of_model")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<List<@Valid ReadOnlyFirst>> arrayArrayOfModel = new ArrayList<>();

  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  public ArrayTest addArrayOfStringItem(String arrayOfStringItem) {
    if (this.arrayOfString == null) {
      this.arrayOfString = new ArrayList<>();
    }
    this.arrayOfString.add(arrayOfStringItem);
    return this;
  }


  public ArrayTest arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  public ArrayTest addArrayArrayOfIntegerItem(List<Long> arrayArrayOfIntegerItem) {
    if (this.arrayArrayOfInteger == null) {
      this.arrayArrayOfInteger = new ArrayList<>();
    }
    this.arrayArrayOfInteger.add(arrayArrayOfIntegerItem);
    return this;
  }


  public ArrayTest arrayArrayOfModel(List<List<@Valid ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  public ArrayTest addArrayArrayOfModelItem(List<@Valid ReadOnlyFirst> arrayArrayOfModelItem) {
    if (this.arrayArrayOfModel == null) {
      this.arrayArrayOfModel = new ArrayList<>();
    }
    this.arrayArrayOfModel.add(arrayArrayOfModelItem);
    return this;
  }



}

