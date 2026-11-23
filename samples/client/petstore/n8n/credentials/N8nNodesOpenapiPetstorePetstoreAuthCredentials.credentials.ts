import type {
	IAuthenticateGeneric,
	ICredentialType,
	INodeProperties,
} from 'n8n-workflow';

/**
 * Generated from one of the OpenAPI spec's declared security schemes. Each declared scheme gets
 * its own credential class/file this way; the generated node's execute() applies whichever of
 * these matches each individual operation's own security requirement (its own declared scheme,
 * or the spec's global default when the operation declares none of its own) - see
 * GenericFunctions.ts's apiRequest. An operation with no security requirement at all, locally or
 * globally, sends an unauthenticated request.
 * 
 * 
 *
 * This scheme declares an OAuth2 flow this generator doesn't auto-wire (not a single
 * `clientCredentials`/`authorizationCode` flow - either a different flow like `implicit`/
 * `password`, or more than one flow declared at once, which is ambiguous: which one should n8n
 * actually perform?), so this collects a manually obtained access token instead.
 * 
 */
export class N8nNodesOpenapiPetstorePetstoreAuthCredentials implements ICredentialType {
	name = 'n8nNodesOpenapiPetstorePetstoreAuthCredentials';

	displayName = "OpenAPI Petstore API - OAuth2";

	properties: INodeProperties[] = [
		{
			displayName: 'Base URL',
			name: 'baseUrl',
			type: 'string',
			default: 'http://petstore.swagger.io/v2',
		},
		{
			displayName: 'Access Token',
			name: 'accessToken',
			type: 'string',
			typeOptions: { password: true },
			default: '',
			description: 'This spec declares an OAuth2 scheme this generator does not auto-wire (see README) - paste a token obtained out of band here for now.',
		},
	];

	authenticate: IAuthenticateGeneric = {
		type: 'generic',
		properties: {
			headers: {
				Authorization: '=Bearer {{$credentials.accessToken}}',
			},
		},
	};
}
