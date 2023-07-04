package org.openapitools.codegen;

import java.util.List;
import java.util.Objects;

public class CodegenEncoding {
    private String contentType;
    private List<CodegenParameter> headers;
    private String style;
    private boolean explode;
    private boolean allowReserved;

    public CodegenEncoding(String contentType, List<CodegenParameter> headers, String style, boolean explode, boolean allowReserved) {
        this.contentType = contentType;
        this.headers = headers;
        this.style = style;
        this.explode = explode;
        this.allowReserved = allowReserved;
    }

    public String getContentType() {
        return contentType;
    }

    public List<CodegenParameter> getHeaders() {
        return headers;
    }

    public String getStyle() {
        return style;
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
                allowReserved == that.getAllowReserved();
    }

    @Override
    public int hashCode() {
        return Objects.hash(contentType, headers, style, explode, allowReserved);
    }
}
