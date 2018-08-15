import {RequestContext} from '../http/http';
// typings for btoa are incorrect
//@ts-ignore
import * as btoa from "btoa";

export abstract class SecurityAuthentication {

	public constructor(private name: string) {
	
	}
	
	/*
	 * 
	 * @return returns the name of the security authentication as specified in OAI
	 */
	public getName(): string {
		return this.name;
	}
	
	public abstract applySecurityAuthentication(context: RequestContext): void;

}

export class NoAuthentication extends SecurityAuthentication {
	
	public constructor() {
		super("_no_auth");
	}
	
	public applySecurityAuthentication(_context: RequestContext) {
	
	}
}

export class APIKeyAuthentication extends SecurityAuthentication {
	
	public constructor(authName: string, private paramName: string, private keyLocation: "query" | "header" | "cookie", private apiKey: string) {
		super(authName);
	}
		
	public applySecurityAuthentication(context: RequestContext) {
		if (this.keyLocation === "header") {
			context.setHeaderParam(this.paramName, this.apiKey);
		} else if (this.keyLocation === "cookie") {
			context.addCookie(this.paramName, this.apiKey);
		} else if (this.keyLocation === "query") {
			context.setQueryParam(this.paramName, this.apiKey);
		}
	}
}
// TODO: guarantee that auth was configured properly

export class HttpBasicAuthentication extends SecurityAuthentication {
	
	public constructor(authName: string, private username: string, private password: string) {
		super(authName);
	}
	
	public applySecurityAuthentication(context: RequestContext) {
		let comb = this.username + ":" + this.password;
		context.setHeaderParam("Authentication", "Basic " + btoa(comb));
	}
}

export class OAuth2Authentication extends SecurityAuthentication {
	public constructor(authName: string) {
		super(authName);
	}
	
	public applySecurityAuthentication(context: RequestContext) {
		// TODO
	}
}

export type AuthMethods = {
		"api_key"?: APIKeyAuthentication,
		"petstore_auth"?: OAuth2Authentication,
}

export type ApiKeyConfiguration = string;
export type HttpBasicConfiguration = { "username": string, "password": string };
export type OAuth2Configuration = string;

export type AuthMethodsConfiguration = { "api_key"?:ApiKeyConfiguration,  "petstore_auth"?:OAuth2Configuration,   }

export function configureAuthMethods(conf: AuthMethodsConfiguration | undefined): AuthMethods {
	let authMethods: AuthMethods = {
	}

	if (!conf) {
		return authMethods;
	}		

	if (conf["api_key"]) {
		authMethods["api_key"] = new APIKeyAuthentication("api_key",  "api_key", "header", <string> conf["api_key"]);
	}

	if (conf["petstore_auth"]) {
		authMethods["petstore_auth"] = new OAuth2Authentication("petstore_auth");
	}

	return authMethods;
}