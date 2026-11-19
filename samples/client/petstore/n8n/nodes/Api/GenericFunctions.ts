import type { IExecuteFunctions, IDataObject, IHttpRequestOptions } from 'n8n-workflow';

/**
 * Builds and sends a single HTTP request against the OpenAPI Petstore API.
 * 
 * 
 * 
 * This spec declares more than one security scheme, so more than one credential type was
 * generated (see credentials/); only the highest-priority one, "apiKeyCredentials", is applied
 * to outgoing requests here by default - see the generated README's "Known limitations".
 * 
 * 
 * 
 * 
 * This spec declares more than one security scheme, so more than one credential type was
 * generated (see credentials/); only the highest-priority one, "petstoreAuthCredentials", is applied
 * to outgoing requests here by default - see the generated README's "Known limitations".
 * 
 * 
 */
export async function apiRequest(
	this: IExecuteFunctions,
	method: string,
	path: string,
	body: IDataObject = {},
	qs: IDataObject = {},
): Promise<unknown> {
	const credentials = await this.getCredentials('apiKeyCredentials');
	const baseUrl = (credentials.baseUrl as string) || 'http://petstore.swagger.io/v2';

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

	return this.helpers.httpRequestWithAuthentication.call(this, 'apiKeyCredentials', options);
}
