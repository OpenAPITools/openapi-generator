package apimodels;

import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * Body2
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class Body2   {
  /**
   * Gets or Sets enumFormStringArray
   */
  public enum EnumFormStringArrayEnum {
    GREATER_THAN(">"),
    
    DOLLAR("$");

    private final String value;

    EnumFormStringArrayEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringArrayEnum fromValue(String text) {
      for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
  }
 
  @JsonProperty("enum_form_string_array")
  private List<EnumFormStringArrayEnum> enumFormStringArray = null;

  /**
   * Form parameter enum test (string)
   */
  public enum EnumFormStringEnum {
    _ABC("_abc"),
    
    _EFG("-efg"),
    
    _XYZ_("(xyz)");

    private final String value;

    EnumFormStringEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringEnum fromValue(String text) {
      for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
  }

  @JsonProperty("enum_form_string")
  private EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;

  public Body2 enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  public Body2 addEnumFormStringArrayItem(EnumFormStringArrayEnum enumFormStringArrayItem) {
    if (enumFormStringArray == null) {
      enumFormStringArray = new ArrayList<>();
    }
    enumFormStringArray.add(enumFormStringArrayItem);
    return this;
  }

   /**
   * Form parameter enum test (string array)
   * @return enumFormStringArray
  **/
    public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }

  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  public Body2 enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }

   /**
   * Form parameter enum test (string)
   * @return enumFormString
  **/
    public EnumFormStringEnum getEnumFormString() {
    return enumFormString;
  }

  public void setEnumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body2 body2 = (Body2) o;
    return Objects.equals(enumFormStringArray, body2.enumFormStringArray) &&
        Objects.equals(enumFormString, body2.enumFormString);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumFormStringArray, enumFormString);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body2 {\n");
    
    sb.append("    enumFormStringArray: ").append(toIndentedString(enumFormStringArray)).append("\n");
    sb.append("    enumFormString: ").append(toIndentedString(enumFormString)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

