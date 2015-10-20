package io.swagger.api;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-10-20T08:49:54.299-07:00")
public class NotFoundException extends ApiException {
	private int code;
	public NotFoundException (int code, String msg) {
		super(code, msg);
		this.code = code;
	}
}
