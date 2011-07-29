package com.wordnik.codegen.resource;

import com.wordnik.codegen.ResourceMethod;
import com.wordnik.codegen.config.DataTypeMappingProvider;
import com.wordnik.codegen.config.NamingPolicyProvider;
import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 7:01 PM
 */
public class Resource {
	
	private String apiVersion;

    //TODO rename the JSON property too after the sandbox var has been renamed
    @JsonProperty("swagrVersion")
    private String swaggerVersion;

    @JsonProperty("apis")
    private List<Endpoint> endPoints = new ArrayList<Endpoint>();

    @JsonProperty("models")
    private ApiModelListWrapper modelListWrapper;

    
    private List<Model> models = new ArrayList<Model>();    

    private String generatedClassName;
    
    private List<ResourceMethod> methods;

    @JsonCreator
    public Resource() {

    }
    
	public String getApiVersion() {
		return apiVersion;
	}

	public void setApiVersion(String apiVersion) {
		this.apiVersion = apiVersion;
	}

    //TODO rename the JSON property too after the sandbox var has been renamed
    @JsonProperty("swagrVersion")
    public String getSwaggerVersion() {
        return swaggerVersion;
    }

    @JsonProperty("swaggerVersion")
    public void setSwaggerVersion(String swaggerVersion) {
        this.swaggerVersion = swaggerVersion;
    }

    @JsonProperty("apis")
	public List<Endpoint> getEndPoints() {
		return endPoints;
	}

    @JsonProperty("apis")
	public void setEndPoints(List<Endpoint> endPoints) {
		this.endPoints = endPoints;
	}

    @JsonProperty("models")
    public ApiModelListWrapper getModelListWrapper() {
        return modelListWrapper;
    }

    @JsonProperty("models")
    public void setModelListWrapper(ApiModelListWrapper modelListWrapper) {
        this.modelListWrapper = modelListWrapper;
    }

	public List<Model> getModels() {
		return models;
	}

	/*public void setModels(List<Model> models) {
		this.models = models;
	}*/
    
	public String generateClassName(NamingPolicyProvider nameGenerator) {
		if (generatedClassName == null) {
			String endPointPath = endPoints.get(0).getPath();
			generatedClassName = nameGenerator.getServiceName(endPointPath);
		}	
		return generatedClassName;
	}
	
	public List<ResourceMethod> generateMethods(Resource resource, DataTypeMappingProvider dataTypeMapper, NamingPolicyProvider nameGenerator) {
		if(methods == null){
			methods = new ArrayList<ResourceMethod>();
			if(getEndPoints() != null) {
				for(Endpoint endpoint: getEndPoints()){
					methods.addAll(endpoint.generateMethods(resource, dataTypeMapper, nameGenerator));
				}
			}
		}
		return methods;
	}

    public void generateModelsFromWrapper(NamingPolicyProvider nameGenerator) {
        String modelName;
        ApiModelDefn modelDefn;
        if (modelListWrapper != null) {
            for (Map.Entry<String, ApiModelDefn> entry : modelListWrapper.getModelList().entrySet()) {
                modelName = entry.getKey();
                modelDefn = entry.getValue();
                models.add (modelDefn.toModel(modelName, nameGenerator) );
            }
        }

    }
	
}
