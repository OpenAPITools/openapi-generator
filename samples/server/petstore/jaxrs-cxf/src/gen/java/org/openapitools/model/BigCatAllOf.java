package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

public class BigCatAllOf  {
  
public enum KindEnum {

LIONS(String.valueOf("lions")), TIGERS(String.valueOf("tigers")), LEOPARDS(String.valueOf("leopards")), JAGUARS(String.valueOf("jaguars"));


    private String value;

    KindEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static KindEnum fromValue(String value) {
        for (KindEnum b : KindEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private KindEnum kind;
 /**
   * Get kind
   * @return kind
  **/
  @JsonProperty("kind")
  public String getKind() {
    if (kind == null) {
      return null;
    }
    return kind.value();
  }

  public void setKind(KindEnum kind) {
    this.kind = kind;
  }

  public BigCatAllOf kind(KindEnum kind) {
    this.kind = kind;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BigCatAllOf {\n");
    
    sb.append("    kind: ").append(toIndentedString(kind)).append("\n");
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

