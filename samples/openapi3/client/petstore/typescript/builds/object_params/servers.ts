import { RequestContext, HttpMethod, Headers } from "./http/http";

export interface BaseServerConfiguration {
    getHeaders(): Headers
    makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext;
    setHeaders(headers: Headers): any;
    setHeaderParam(key: string, value: string): void
}

/**
 *
 * Represents the configuration of a server including its
 * url template and variable configuration based on the url.
 *
 */
export class ServerConfiguration<T extends { [key: string]: string }> implements BaseServerConfiguration {
    public constructor(private url: string, private variableConfiguration: T, private headers: Headers = {}) {}

    /**
     * Sets the value of the variables of this server.
     *
     * @param variableConfiguration a partial variable configuration for the variables contained in the url
     */
    public setVariables(variableConfiguration: Partial<T>) {
        Object.assign(this.variableConfiguration, variableConfiguration);
    }

    public getConfiguration(): T {
        return this.variableConfiguration
    }

    public setHeaders(headers: Headers): void {
        Object.assign(this.headers, headers);
    }

    public getHeaders(): Headers {
        return this.headers
    }

    public setHeaderParam(key: string, value: string): void  {
        this.headers[key] = value;
    }

    private getUrl() {
        let replacedUrl = this.url;
        for (const key in this.variableConfiguration) {
            var re = new RegExp("{" + key + "}","g");
            replacedUrl = replacedUrl.replace(re, this.variableConfiguration[key]);
        }
        return replacedUrl
    }

    /**
     * Creates a new request context for this server using the url with variables
     * replaced with their respective values and the endpoint of the request appended.
     *
     * @param endpoint the endpoint to be queried on the server
     * @param httpMethod httpMethod to be used
     *
     */
    public makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext {
        return new RequestContext(this.getUrl() + endpoint, httpMethod, this.headers);
    }
}

export const server1 = new ServerConfiguration<{  }>("http://petstore.swagger.io/v2", {  })

export const servers = [server1];
