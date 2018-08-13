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
	
	public constructor(authName: string, private paramName: string, private apiKey: string, private keyLocation: "query" | "header" | "cookie") {
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

export class HttpBasicAuthentication extends SecurityAuthentication {

	public constructor(authName: string, private username: string, private password: string) {
		super(authName);
	}

	public applySecurityAuthentication(context: RequestContext) {
		let comb = this.username + ":" + this.password;
		context.setHeaderParam("Authentication", "Basic " + btoa(comb));
	}
}