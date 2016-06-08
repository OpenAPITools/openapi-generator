package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import com.google.gson.annotations.SerializedName;


/**
 * ReadOnlyFirst
 */
public class ReadOnlyFirst   {
    @SerializedName("bar")
    private String bar = null;
    @SerializedName("baz")
    private String baz = null;

    /**
     * Get bar
     * @return bar
     **/
    @ApiModelProperty(value = "")
    public String getBar() {
        return bar;
    }

    /**
     * Get baz
     * @return baz
     **/
    @ApiModelProperty(value = "")
    public String getBaz() {
        return baz;
    }

    /**
     * Set baz
     *
     * @param baz baz
     */
    public void setBaz(String baz) {
        this.baz = baz;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ReadOnlyFirst readOnlyFirst = (ReadOnlyFirst) o;
        return Objects.equals(this.bar, readOnlyFirst.bar) &&
        Objects.equals(this.baz, readOnlyFirst.baz);
    }

    @Override
    public int hashCode() {
        return Objects.hash(bar, baz);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ReadOnlyFirst {\n");
        
        sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
        sb.append("    baz: ").append(toIndentedString(baz)).append("\n");
        sb.append("}");
        return sb.toString();
    }

    /**
     * Convert the given object to string with each line indented by 4 spaces
     * (except the first line).
     *
     * @param o Object to be converted to indented string
     */
    private String toIndentedString(Object o) {
        if (o == null) {
            return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

