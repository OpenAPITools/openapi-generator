package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.DeprecatedObject;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("ObjectWithDeprecatedFields")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class ObjectWithDeprecatedFields  implements Serializable {
  private String uuid;
  private BigDecimal id;
  private DeprecatedObject deprecatedRef;
  private @Valid List<String> bars = new ArrayList<>();

  protected ObjectWithDeprecatedFields(ObjectWithDeprecatedFieldsBuilder<?, ?> b) {
    this.uuid = b.uuid;
    this.id = b.id;
    this.deprecatedRef = b.deprecatedRef;
    this.bars = b.bars;
  }

  public ObjectWithDeprecatedFields() {
  }

  /**
   **/
  public ObjectWithDeprecatedFields uuid(String uuid) {
    this.uuid = uuid;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public String getUuid() {
    return uuid;
  }

  @JsonProperty("uuid")
  public void setUuid(String uuid) {
    this.uuid = uuid;
  }

  /**
   **/
  public ObjectWithDeprecatedFields id(BigDecimal id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  @Valid public BigDecimal getId() {
    return id;
  }

  @JsonProperty("id")
  public void setId(BigDecimal id) {
    this.id = id;
  }

  /**
   **/
  public ObjectWithDeprecatedFields deprecatedRef(DeprecatedObject deprecatedRef) {
    this.deprecatedRef = deprecatedRef;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("deprecatedRef")
  @Valid public DeprecatedObject getDeprecatedRef() {
    return deprecatedRef;
  }

  @JsonProperty("deprecatedRef")
  public void setDeprecatedRef(DeprecatedObject deprecatedRef) {
    this.deprecatedRef = deprecatedRef;
  }

  /**
   **/
  public ObjectWithDeprecatedFields bars(List<String> bars) {
    this.bars = bars;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("bars")
  public List<String> getBars() {
    return bars;
  }

  @JsonProperty("bars")
  public void setBars(List<String> bars) {
    this.bars = bars;
  }

  public ObjectWithDeprecatedFields addBarsItem(String barsItem) {
    if (this.bars == null) {
      this.bars = new ArrayList<>();
    }

    this.bars.add(barsItem);
    return this;
  }

  public ObjectWithDeprecatedFields removeBarsItem(String barsItem) {
    if (barsItem != null && this.bars != null) {
      this.bars.remove(barsItem);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ObjectWithDeprecatedFields objectWithDeprecatedFields = (ObjectWithDeprecatedFields) o;
    return Objects.equals(this.uuid, objectWithDeprecatedFields.uuid) &&
        Objects.equals(this.id, objectWithDeprecatedFields.id) &&
        Objects.equals(this.deprecatedRef, objectWithDeprecatedFields.deprecatedRef) &&
        Objects.equals(this.bars, objectWithDeprecatedFields.bars);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, id, deprecatedRef, bars);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ObjectWithDeprecatedFields {\n");
    
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    deprecatedRef: ").append(toIndentedString(deprecatedRef)).append("\n");
    sb.append("    bars: ").append(toIndentedString(bars)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }


  public static ObjectWithDeprecatedFieldsBuilder<?, ?> builder() {
    return new ObjectWithDeprecatedFieldsBuilderImpl();
  }

  private static final class ObjectWithDeprecatedFieldsBuilderImpl extends ObjectWithDeprecatedFieldsBuilder<ObjectWithDeprecatedFields, ObjectWithDeprecatedFieldsBuilderImpl> {

    @Override
    protected ObjectWithDeprecatedFieldsBuilderImpl self() {
      return this;
    }

    @Override
    public ObjectWithDeprecatedFields build() {
      return new ObjectWithDeprecatedFields(this);
    }
  }

  public static abstract class ObjectWithDeprecatedFieldsBuilder<C extends ObjectWithDeprecatedFields, B extends ObjectWithDeprecatedFieldsBuilder<C, B>>  {
    private String uuid;
    private BigDecimal id;
    private DeprecatedObject deprecatedRef;
    private List<String> bars = new ArrayList<>();
    protected abstract B self();

    public abstract C build();

    public B uuid(String uuid) {
      this.uuid = uuid;
      return self();
    }
    public B id(BigDecimal id) {
      this.id = id;
      return self();
    }
    public B deprecatedRef(DeprecatedObject deprecatedRef) {
      this.deprecatedRef = deprecatedRef;
      return self();
    }
    public B bars(List<String> bars) {
      this.bars = bars;
      return self();
    }
  }
}

