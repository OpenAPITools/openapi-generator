import {RequestContext, HttpMethod} from './http/http';

export class ServerConfiguration {
	
    public constructor(private url: string) {
    }
    
	public makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext {
		return new RequestContext(this.url + endpoint, httpMethod);		
	}
}

export const servers = [
	new ServerConfiguration("/"),
]
