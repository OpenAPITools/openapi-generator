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
	body: IDataObject | unknown[] | string | number | boolean = {},
	qs: IDataObject = {},
	headers: IDataObject = {},
	form: IDataObject = {},
	credentialName?: string,
): Promise<unknown> {
	let baseUrl = 'http://petstore.swagger.io/v2';

	if (credentialName) {
		const credentials = await this.getCredentials(credentialName);
		baseUrl = (credentials.baseUrl as string) || baseUrl;
	}
	else {
		// This operation itself needs no authentication, but the package ships at least one
		// credential type (see credentials/) - if the user has configured it on this node anyway,
		// best-effort use its Base URL override instead of always falling back to the spec's
		// hard-coded default, which has no other way of being overridden for unauthenticated calls.
		try {
			const fallbackCredentials = await this.getCredentials('n8nNodesOpenapiPetstoreApiKeyCredentials');
			baseUrl = (fallbackCredentials.baseUrl as string) || baseUrl;
		} catch {
			// no 'n8nNodesOpenapiPetstoreApiKeyCredentials' credential configured on this node; keep the spec's default.
		}
	}
	baseUrl = baseUrl.replace(/\/+$/, '');

	const options: IHttpRequestOptions = {
		method: method as IHttpRequestOptions['method'],
		url: baseUrl + path,
		qs,
		body,
		json: true,
	};

	const isBodyEmpty = typeof body === 'object' && body !== null && Object.keys(body as object).length === 0;
	if (isBodyEmpty) {
		delete options.body;
	}

	const hasForm = Object.keys(form).length > 0;
	if (hasForm) {
		options.body = Object.entries(form)
			.map(([key, value]) => encodeURIComponent(key) + '=' + encodeURIComponent(String(value)))
			.join('&');
		options.json = false;
		headers['Content-Type'] = 'application/x-www-form-urlencoded';
	}

	if (Object.keys(headers).length > 0) {
		options.headers = headers;
	}

	if (credentialName) {
		return this.helpers.httpRequestWithAuthentication.call(this, credentialName, options);
	}
	return this.helpers.httpRequest(options);
}
