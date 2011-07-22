package com.wordnik.codegen.resource;

import com.wordnik.codegen.Argument;
import com.wordnik.codegen.Method;
import com.wordnik.codegen.config.CodeGenConfig;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * User: ramesh
 * Date: 3/31/11
 * Time: 7:54 AM
 */
public class EndpointOperation {

	public static String PARAM_TYPE_QUERY = "query";
	public static String PARAM_TYPE_PATH = "path";
	public static String PARAM_TYPE_BODY = "body";
	public static String PARAM_TYPE_HEADER = "header";
	private static String AUTH_TOKEN_PARAM_NAME = "auth_token";
	private static String API_KEY_PARAM_NAME = "api_key";	
	private static String FORMAT_PARAM_NAME = "format";	
	
	private static String AUTH_TOKEN_ARGUMENT_NAME = "authToken";	
	
    private String httpMethod;

    private String summary = "";

    private String notes = "";

    private boolean open;

    @Deprecated
    private List<Response> response;

    private String responseClass;

    private List<Parameter> parameters;
    
    private boolean deprecated;
    
    private Method method;

    private List<String> tags;

    @Deprecated
    private String suggestedName;

    private String nickname;
    
	public String getHttpMethod() {
		return httpMethod;
	}

	public void setHttpMethod(String httpMethod) {
		this.httpMethod = httpMethod;
	}

	public String getSummary() {
		return summary;
	}

	public void setSummary(String summary) {
		this.summary = summary;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public boolean isOpen() {
		return open;
	}

	public void setOpen(boolean open) {
		this.open = open;
	}

	public List<Response> getResponse() {
		return response;
	}

	public void setResponse(List<Response> response) {
		this.response = response;
	}

    public String getResponseClass() {
        return responseClass;
    }

    public void setResponseClass(String responseClass) {
        this.responseClass = responseClass;
        this.setResponse(new ArrayList<Response>());
        Response response = new Response();
        response.setValueType(this.responseClass);
        this.getResponse().add(response);
    }

	public List<Parameter> getParameters() {
		return parameters;
	}

	public void setParameters(List<Parameter> parameters) {
		this.parameters = parameters;
	}
	
	public boolean isDeprecated() {
		return deprecated;
	}

	public void setDeprecated(boolean deprecated) {
		this.deprecated = deprecated;
	}
	
	public String getSuggestedName() {
		return suggestedName;
	}

    public void setSuggestedName(String suggestedName) {
        this.suggestedName = suggestedName;
    }

    public void setNickname(String nickname) {
		this.nickname = nickname;
        this.suggestedName = nickname;
	}

	public String getNickname() {
		return nickname;
	}

    public List<String> getTags() {
        return tags;
    }

    public void setTags(List<String> tags) {
        this.tags = tags;
    }


	public Method generateMethod(Endpoint endPoint, Resource resource, CodeGenConfig config) {
		if(method == null){
			method = new Method();
			//add method description
			method.setDescription(this.getSummary() + "\n " + getNotes());
			
			//add method name
			//get resource path for making web service call
			/**
			 * Logic for method names
			 * 1. remove all path parameters
			 * 2. Remove format path parameter
			 * 3. For POST add save 
			 * 4. For PUT add update
			 * 5. For DELETE add delete
			 * 6. For GET add get
			 * 7. Concatenate rest of the path with init caps
			 * 8. 
			 */

			String inputobjectName = config.getNameGenerator().getInputObjectName(resource.generateClassName(config), endPoint.getPath());
			
			String[] pathElements = endPoint.getPath().split("/");
			StringBuilder urlPath = new StringBuilder("");
			if(pathElements != null){
				for(int i=0; i < pathElements.length; i++){
					String pathElement  = pathElements[i];
					if(pathElement != null && pathElement.length() > 0) {
						int position = pathElement.indexOf("{");
						if(urlPath.length() > 0) {
							urlPath.append("+");
						}
						if(position < 0) {
							urlPath.append("\"/"+pathElement+"\"");
						}else if (position == 0) {
							urlPath.append("\"/\"+"+pathElement.substring(1, pathElement.length()-1));
						}else{
							urlPath.append("\"/"+pathElement.replace("{format}", "json")+"\"");
						}
					}
				}
			}
			method.setResourcePath(endPoint.getPath());
			method.setName(config.getNameGenerator().getMethodName(endPoint.getPath(), this.getSuggestedName()));
			
			//create method argument
			/**
			 * 1. API token need not be included as that is always added to the calls as HTTP headers
			 * 2. We need to handle auth token specially, hence need to differentiate that
			 * 3. Query parameters needs to be added as query string hence need to separate them out
			 * 4. Post parameters are usually WordnikObjects, hence we need to handle them separately 
			 */
			List<String> argNames = new ArrayList<String>();
			if(this.getParameters() != null) {
				List<Argument> arguments = new ArrayList<Argument>();
				List<Argument> queryParams= new ArrayList<Argument>();
				List<Argument> pathParams= new ArrayList<Argument>();
				method.setArguments(arguments);
				method.setQueryParameters(queryParams);
				method.setPathParameters(pathParams);
				
				for(Parameter parameter: this.getParameters()){
					if(!argNames.contains(parameter.getName())) {
						argNames.add(parameter.getName());
						Argument anArgument = new Argument();
						anArgument.setAllowedValues(parameter.getAllowedValuesString());
						//check if arguments has auth token
						if(parameter.getParamType().equalsIgnoreCase(PARAM_TYPE_HEADER) &&
								parameter.getName().equals(AUTH_TOKEN_PARAM_NAME)){
							method.setAuthToken(true);
							anArgument.setName(AUTH_TOKEN_ARGUMENT_NAME);
							anArgument.setDataType(Argument.ARGUMENT_STRING);
							anArgument.setDescription(parameter.getDescription());
							arguments.add(anArgument);
						}else if(parameter.getParamType().equalsIgnoreCase(PARAM_TYPE_HEADER) &&
								parameter.getName().equals(API_KEY_PARAM_NAME)){
							//do nothing for API key parameter as all calls will automatically add API KEY to the http headers
						}else if (parameter.getParamType().equalsIgnoreCase(PARAM_TYPE_PATH) && 
								!parameter.getName().equalsIgnoreCase(FORMAT_PARAM_NAME)) {
							anArgument.setName(parameter.getName());
							anArgument.setDataType(Argument.ARGUMENT_STRING);
							anArgument.setDescription(parameter.getDescription());
							arguments.add(anArgument);
							pathParams.add(anArgument);
						}else if (parameter.getParamType().equalsIgnoreCase(PARAM_TYPE_QUERY)) {
							anArgument.setName(parameter.getName());
							anArgument.setDataType(Argument.ARGUMENT_STRING);
							anArgument.setDescription(parameter.getDescription());
							queryParams.add(anArgument);
							arguments.add(anArgument);
						}else if (parameter.getParamType().equalsIgnoreCase(PARAM_TYPE_BODY)) {
							if(parameter.getName() == null) {
								parameter.setName("postObject");
							}
							anArgument.setName(parameter.getName());
							anArgument.setDataType(config.getDataTypeMapper().getReturnValueType(parameter.getDataType()));
							anArgument.setDescription(parameter.getDescription());
							arguments.add(anArgument);
                            method.setPostObject(true);
						}
                        anArgument.setInputModelClassArgument(inputobjectName, config);
					}
				}
			}
			
			//check for number of arguments, if we have more than 4 then send the arguments as input object
			if(method.getArguments() != null && method.getArguments().size() > 4){
                List<Argument> arguments = new ArrayList<Argument>();
				Model modelforMethodInput = new Model();
				modelforMethodInput.setName(inputobjectName);
				List<Parameter> fields = new ArrayList<Parameter>();
				for(Argument argument: method.getArguments()){
                    if(!argument.getName().equals("postObject") && !argument.getName().equals("authToken")){
                        Parameter aParameter = new Parameter();
                        aParameter.setAllowedValues(argument.getAllowedValues());
                        aParameter.setDescription(argument.getDescription());
                        aParameter.setName(argument.getName());
                        aParameter.setParamType(argument.getDataType());
                        fields.add(aParameter);
                    }else{
                        arguments.add(argument);
                    }
				}
				modelforMethodInput.setFields(fields);
				
				Argument anArgument = new Argument();
				anArgument.setDataType(inputobjectName);
				anArgument.setName(config.getNameGenerator().convertToMethodNameFormat(inputobjectName));
				arguments.add(anArgument);
				method.setArguments(arguments);
				method.setInputModel(modelforMethodInput);
			}
			
			List<String> argumentDefinitions = new ArrayList<String>();
			List<String> argumentNames = new ArrayList<String>();
            if (method.getArguments() != null && method.getArguments().size() > 0) {
                for(Argument arg: method.getArguments()) {
                    if(!arg.getName().equalsIgnoreCase(FORMAT_PARAM_NAME)){
                        argumentDefinitions.add(arg.getDataType() + " " + arg.getName());
                        argumentNames.add(arg.getName());
                    }
                }
                method.setArgumentDefinitions(argumentDefinitions);
                method.setArgumentNames(argumentNames);
            }

            //get method type
			method.setMethodType(this.getHttpMethod());
			
			//get return value
			//private String returnValue;
			List<Response> response = this.getResponse();

			method.setReturnValue(config.getDataTypeMapper().getReturnValueType(response.get(0).getValueType()));
			method.setReturnClassName(config.getDataTypeMapper().getReturnClassType(response.get(0).getValueType()));

			
			//get description string for exception			
			method.setExceptionDescription(calculateExceptionMessage());
		}
		return method;
	} 	

	/**
	 * Each operation can have one or many error responses Concatenate all the error responses and create on string
	 * @return
	 */
	private String calculateExceptionMessage() {
		StringBuilder errorMessage = new StringBuilder();
		if(this.getResponse() != null) {
			for(Response response: this.getResponse()) {
				if(response.getErrorResponses() != null) {
					for(ErrorResponse errorResponse : response.getErrorResponses()){
						errorMessage.append(errorResponse.getCode() + " - " + errorResponse.getReason() +" ");
					}
				}
			}
		}
		return errorMessage.toString();
	}

}
