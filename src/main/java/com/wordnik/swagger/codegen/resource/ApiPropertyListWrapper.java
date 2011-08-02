package com.wordnik.swagger.codegen.resource;

import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import org.codehaus.jackson.annotate.JsonAnyGetter;
import org.codehaus.jackson.annotate.JsonAnySetter;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class ApiPropertyListWrapper implements Serializable
{

    private Map<String, ApiPropertyDefn> propertyList = new HashMap<String, ApiPropertyDefn>();

    @JsonAnyGetter
    public Map<String, ApiPropertyDefn> getPropertyList() {
        return this.propertyList;
    }

    @JsonAnySetter
    public void setPropertyList(String name, ApiPropertyDefn value) {
        this.propertyList.put(name, value);
    }

    public List<ModelField> toFieldList(NamingPolicyProvider nameGenerator) {
        List<ModelField> fields = new ArrayList<ModelField>();
        ModelField field;

        String propertyName;
        ApiPropertyDefn propertyDefn;
        for(Map.Entry<String, ApiPropertyDefn> propertyDefnEntry : this.getPropertyList().entrySet()) {
            propertyName = propertyDefnEntry.getKey();
            propertyDefn = propertyDefnEntry.getValue();

            field = new ModelField();
            field.setName(propertyName);
            //TODO - need to handle this via the nameGenerator which will do this in case the propertyName is a key word in the language
            if(propertyName.equals("enum") || propertyName.equals("default")){
                field.setName(propertyName+"Value");
            }
            field.setDescription(propertyDefn.getDescription());
            //field.setAllowableValues(propertyDefn.getPossibleValues()); //TODO
            //field.setDataType(propertyDefn.getType());  //TODO - verify if this is needed for a model field - paramType is set
            field.setParamType(propertyDefn.getType());
            if(propertyDefn.getType().equals("array")){
                String arrayItemType = propertyDefn.getItems().getType();
                if(propertyDefn.getItems().getAdditionalProperties().get("$ref") != null) {
                    arrayItemType = (String) propertyDefn.getItems().getAdditionalProperties().get("$ref");
                }
                field.setParamType("List[" + nameGenerator.applyClassNamingPolicy(arrayItemType) + "]");
            }
            field.setDefaultValue(propertyDefn.getDefaultValue());
            field.setInternalDescription(propertyDefn.getNotes());
            field.setParamAccess(propertyDefn.getAccess());
            field.setRequired(propertyDefn.isRequired());
            //field.setWrapperName(propertyDefn);
            fields.add(field);
        }
        return fields;
    }

}
