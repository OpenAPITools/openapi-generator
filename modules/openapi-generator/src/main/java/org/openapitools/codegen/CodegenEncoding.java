package org.openapitools.codegen;

import lombok.Getter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class CodegenEncoding {
    @Getter private String contentType;
    @Getter private List<CodegenParameter> headers;
    @Getter private String style;
    private boolean explode;
    private boolean allowReserved;
    @Getter public Map<String, Object> vendorExtensions = new HashMap<>();

    public CodegenEncoding(String contentType, List<CodegenParameter> headers, String style, boolean explode, boolean allowReserved) {
        this.contentType = contentType;
        this.headers = headers;
        this.style = style;
        this.explode = explode;
        this.allowReserved = allowReserved;
    }

    public boolean getExplode() {
        return explode;
    }

    public boolean getAllowReserved() {
        return allowReserved;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenEncoding{");
        sb.append("contentType=").append(contentType);
        sb.append(", headers=").append(headers);
        sb.append(", style=").append(style);
        sb.append(", explode=").append(explode);
        sb.append(", allowReserved=").append(allowReserved);
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append('}');
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenEncoding that = (CodegenEncoding) o;
        return contentType == that.getContentType() &&
                Objects.equals(headers, that.getHeaders()) &&
                style == that.getStyle() &&
                explode == that.getExplode() &&
                allowReserved == that.getAllowReserved() &&
                Objects.equals(vendorExtensions, that.vendorExtensions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contentType, headers, style, explode, allowReserved, vendorExtensions);
    }
}
