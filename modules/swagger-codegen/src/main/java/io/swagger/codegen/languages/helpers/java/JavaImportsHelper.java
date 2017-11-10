package io.swagger.codegen.languages.helpers.java;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class JavaImportsHelper implements Helper<Map<String, Object>> {

    public static final String NAME = "imports";

    @Override
    public CharSequence apply(Map<String, Object> templateData, Options options) throws IOException {
        if (options == null) {
            return null;
        }
        final List<Map<String, String>> imports = options.get("imports");
        if (imports == null || imports.isEmpty()) {
            return null;
        }
        final StringBuilder builder = new StringBuilder();

        boolean supportJava6 = Boolean.valueOf(String.valueOf(templateData.get("supportJava6")));
        if (supportJava6) {
            builder.append("import org.apache.commons.lang3.ObjectUtils;\n");
        } else {
            builder.append("import java.util.Objects;\n");
        }

        for (Map<String, String> importMap : imports) {
            builder.append("import ");
            builder.append(importMap.get("import"));
            builder.append(";\n");
        }
        boolean serializableMode = Boolean.valueOf(String.valueOf(templateData.get("serializableModel")));
        boolean jackson = Boolean.valueOf(String.valueOf(templateData.get("jackson")));
        boolean withXml = Boolean.valueOf(String.valueOf(templateData.get("withXml")));
        boolean parcelableModel = Boolean.valueOf(String.valueOf(templateData.get("parcelableModel")));
        boolean useBeanValidation = Boolean.valueOf(String.valueOf(templateData.get("useBeanValidation")));
        if (serializableMode) {
            builder.append("import java.io.Serializable;\n");
        }
        if (jackson && withXml) {
            builder.append("import com.fasterxml.jackson.dataformat.xml.annotation.*;\n");
        }
        if (withXml) {
            builder.append("import javax.xml.bind.annotation.*;\n");
        }
        if (parcelableModel) {
            builder.append("import android.os.Parcelable;\n");
            builder.append("import android.os.Parcel;\n");
        }
        if (useBeanValidation) {
            builder.append("import javax.validation.constraints.*;\n");
            builder.append("import javax.validation.Valid;\n");
        }
        return builder.toString();
    }
}
