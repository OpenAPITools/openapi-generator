import type { IExecuteFunctions, IDataObject, IHttpRequestOptions } from 'n8n-workflow';

/**
 * Builds and sends a single HTTP request against the OpenAPI Petstore API.
 * 
 * 
 * 
 * 
 * 
 * This spec declares more than one security scheme, so more than one credential type was
 * generated (see credentials/). Each call site passes the credential name appropriate for
 * that operation's own security requirement (see node.mustache's execute()) - its own declared
 * scheme, or the spec's global default when the operation declares none of its own. An
 * operation with no security requirement at all, locally or globally, passes an empty
 * credential name here, and this function sends the request unauthenticated.
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 */
export async function apiRequest(
	this: IExecuteFunctions,
	method: string,
	path: string,
	body: IDataObject = {},
	qs: IDataObject = {},
	credentialName?: string,
): Promise<unknown> {
	let baseUrl = 'http://petstore.swagger.io/v2';

	if (credentialName) {
		const credentials = await this.getCredentials(credentialName);
		baseUrl = (credentials.baseUrl as string) || baseUrl;
	}

	const options: IHttpRequestOptions = {
		method: method as IHttpRequestOptions['method'],
		url: baseUrl + path,
		qs,
		body,
		json: true,
	};

	if (Object.keys(body).length === 0) {
		delete options.body;
	}

	if (credentialName) {
		return this.helpers.httpRequestWithAuthentication.call(this, credentialName, options);
	}
	return this.helpers.httpRequest(options);
}
