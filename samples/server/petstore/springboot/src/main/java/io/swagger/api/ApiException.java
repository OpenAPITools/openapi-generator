package io.swagger.api;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringBootServerCodegen", date = "2016-06-06T14:29:50.468+02:00")
public class ApiException extends Exception{
	private int code;
	public ApiException (int code, String msg) {
		super(msg);
		this.code = code;
	}
}
