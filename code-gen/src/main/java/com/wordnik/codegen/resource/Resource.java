package com.wordnik.codegen.resource;

import com.wordnik.codegen.ResourceMethod;
import com.wordnik.codegen.config.CodeGenConfig;
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

    @JsonProperty("swagrVersion")
    private String swagrVersion;

    @JsonProperty("apis")
    private List<Endpoint> endPoints = new ArrayList<Endpoint>();

    @JsonProperty("models")
    private ApiModelListWrapper modelListWrapper;

    
    private List<Model> models = new ArrayList<Model>();    

    private String generatedClassName;
    
    private List<ResourceMethod> methods;

    @JsonCreator
    public Resource() {//@JsonProperty("models") ApiModelListWrapper modelListWrapper, @JsonProperty("apis") List<Endpoint> endPoints)

    }
    
	public String getApiVersion() {
		return apiVersion;
	}

	public void setApiVersion(String apiVersion) {
		this.apiVersion = apiVersion;
	}

    @JsonProperty("swagrVersion")
    public String getSwagrVersion() {
        return swagrVersion;
    }

    @JsonProperty("swagrVersion")
    public void setSwagrVersion(String swagrVersion) {
        this.swagrVersion = swagrVersion;
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
    
	public String generateClassName(CodeGenConfig config) {
		if (generatedClassName == null) {
			String endPointPath = endPoints.get(0).getPath();
			generatedClassName = config.getNameGenerator().getServiceName(endPointPath);
		}	
		return generatedClassName;
	}
	
	public List<ResourceMethod> generateMethods(Resource resource, CodeGenConfig config) {
		if(methods == null){
			methods = new ArrayList<ResourceMethod>();
			if(getEndPoints() != null) {
				for(Endpoint endpoint: getEndPoints()){
					methods.addAll(endpoint.generateMethods(resource, config));
				}
			}
		}
		return methods;
	}

    public void generateModelsFromWrapper(CodeGenConfig config) {
        String modelName;
        ApiModelDefn modelDefn;
        if (modelListWrapper != null) {
            for (Map.Entry<String, ApiModelDefn> entry : modelListWrapper.getModelList().entrySet()) {
                modelName = entry.getKey();
                modelDefn = entry.getValue();
                models.add (modelDefn.toModel(modelName, config) );
            }
        }

    }
	
}
