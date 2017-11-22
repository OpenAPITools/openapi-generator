package io.swagger.codegen.languages.helpers.java;

import com.github.jknack.handlebars.Options;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.languages.JavaClientCodegen;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class JavaHelper {

    public CharSequence getClassDefinition(CodegenModel codegenModel, Options options) throws IOException {
        final Boolean serializableModel = Boolean.valueOf(String.valueOf(options.get(CodegenConstants.SERIALIZABLE_MODEL)));
        final Boolean parceableModel = Boolean.valueOf(String.valueOf(options.get(JavaClientCodegen.PARCELABLE_MODEL)));
        final StringBuilder builder = new StringBuilder();
        builder.append(codegenModel.classname);
        if (StringUtils.isNotBlank(codegenModel.parent)) {
            builder.append(StringUtils.SPACE);
            builder.append("extends ");
            builder.append(codegenModel.parent);
        }
        if (parceableModel && serializableModel) {
            builder.append(" implements Parcelable, Serializable");
        } else {
            if (serializableModel) {
                builder.append(" implements Serializable");
            }
        }
        return builder.toString();
    }

    public CharSequence getJavaProperty(CodegenProperty codegenProperty, Options options) throws IOException {
        final StringBuilder builder = new StringBuilder();
        if (codegenProperty.getIsContainer()) {
            builder.append(codegenProperty.getDatatypeWithEnum());
            builder.append(StringUtils.SPACE);
            builder.append(codegenProperty.getName());
            builder.append(" = ");
            if (codegenProperty.getRequired()) {
                builder.append(codegenProperty.getDefaultValue());
            } else {
                builder.append("null");
            }
            return builder.toString();
        }
        return String.format("%s %s = %s", codegenProperty.getDatatypeWithEnum(), codegenProperty.getName(), codegenProperty.getDefaultValue());
    }

    public CharSequence getModelImports(Map<String, Object> templateData, Options options) throws IOException {
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

    public CharSequence getXmlAttributeName(String xmlName, String baseName) {
        if (StringUtils.isNotBlank(xmlName)) {
            return xmlName;
        }
        return baseName;
    }

    public CharSequence getXmlElementName(String xmlNamespace, String  xmlName, String baseName) {
        StringBuilder builder = new StringBuilder();
        if (StringUtils.isNotBlank(xmlNamespace)) {
            builder.append("namespace=\"");
            builder.append(xmlNamespace);
            builder.append("\", ");
        }
        builder.append("name=\"");
        if (StringUtils.isNotBlank(xmlName)) {
            builder.append(xmlName);
        } else {
            builder.append(baseName);
        }
        builder.append("\"");
        return builder.toString();
    }

    public CharSequence getJacksonXmlProperty(boolean isXmlAttribute, String xmlNamespace, String xmlName, String baseName) {
        StringBuilder builder = new StringBuilder();
        if (isXmlAttribute) {
            builder.append("isAttribute = true, ");
        }
        if (StringUtils.isNotBlank(xmlNamespace)) {
            builder.append("namespace=\"");
            builder.append(xmlNamespace);
            builder.append("\", ");
        }
        builder.append("localName = \"");
        if (StringUtils.isNotBlank(xmlName)) {
            builder.append(xmlName);
        } else {
            builder.append(baseName);
        }
        builder.append("\"");
        return builder.toString();
    }

    public CharSequence getJacksonXmlElementWrapper(boolean isXmlWrapped, String xmlNamespace, String xmlName, String baseName) {
        StringBuilder builder = new StringBuilder();
        builder.append("useWrapping = ");
        builder.append(isXmlWrapped);
        builder.append(", ");
        if (StringUtils.isNotBlank(xmlNamespace)) {
            builder.append("namespace=\"");
            builder.append(xmlNamespace);
            builder.append("\", ");
        }
        builder.append("localName = \"");
        if (StringUtils.isNotBlank(xmlName)) {
            builder.append(xmlName);
        } else {
            builder.append(baseName);
        }
        builder.append("\"");
        return builder.toString();
    }
}
