package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class BigCatAllOf  {
  
@XmlType(name="KindEnum")
@XmlEnum(String.class)
public enum KindEnum {

@XmlEnumValue("lions") LIONS(String.valueOf("lions")), @XmlEnumValue("tigers") TIGERS(String.valueOf("tigers")), @XmlEnumValue("leopards") LEOPARDS(String.valueOf("leopards")), @XmlEnumValue("jaguars") JAGUARS(String.valueOf("jaguars"));


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

