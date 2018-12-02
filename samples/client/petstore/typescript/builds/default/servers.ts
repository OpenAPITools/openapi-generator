import {RequestContext, HttpMethod} from './http/http';

export class ServerConfiguration<T> {
	
    public constructor(private url: string, private variableConfiguration: T) {
    }
	
	public setVariables(variableConfiguration: Partial<T>) {
		for (const key in variableConfiguration) {
			const val = variableConfiguration[key]
			// We know that val isn't undefined here - hopefully
			if (val !== undefined) {
				this.variableConfiguration[key] = val as T[Extract<keyof T, string>];
			}
		}
	}

	public getConfiguration(): T {
		return this.variableConfiguration
	}

	private getUrl() {
		let replacedUrl = this.url;
		for (const key in this.variableConfiguration) {
			var re = new RegExp("{" + key + "}","g");
			replacedUrl = replacedUrl.replace(re, this.variableConfiguration[key].toString());
		}
		return replacedUrl
	}
	
	public makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext {
		return new RequestContext(this.getUrl() + endpoint, httpMethod);		
	}
}

export const server1 = new ServerConfiguration<{  }>("http://petstore.swagger.io/v2", {  })
