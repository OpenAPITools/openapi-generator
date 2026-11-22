import type {
	IAuthenticateGeneric,
	ICredentialType,
	INodeProperties,
} from 'n8n-workflow';

/**
 * Generated from one of the OpenAPI spec's declared security schemes. Each declared scheme gets
 * its own credential class/file this way (see data-model.md "CredentialDefinition"); the
 * generated node's execute() applies whichever of these matches each individual operation's own
 * security requirement (its own declared scheme, or the spec's global default when the
 * operation declares none of its own) - see GenericFunctions.ts's apiRequest. An operation with
 * no security requirement at all, locally or globally, sends an unauthenticated request.
 *
 * For an OAuth2 scheme declaring exactly one of the `clientCredentials`/`authorizationCode`
 * flows, this extends n8n's own generic `oAuth2Api` credential type (pre-filled with the
 * spec's declared authorization/token URLs and scopes) instead of collecting a manually
 * obtained access token, per research.md §3 / spec.md Assumptions ("common flows ... are
 * supported"). Any other flow (`implicit`, `password`), or a scheme declaring more than one
 * flow at once (ambiguous - which one should n8n actually perform?), keeps the manual-token
 * fallback below instead.
 */
export class OpenapiPetstoreApiKeyCredentials implements ICredentialType {
	name = 'openapiPetstoreApiKeyCredentials';

	displayName = "OpenAPI Petstore API - API Key";

	properties: INodeProperties[] = [
		{
			displayName: 'Base URL',
			name: 'baseUrl',
			type: 'string',
			default: 'http://petstore.swagger.io/v2',
		},
		{
			displayName: 'API Key',
			name: 'apiKey',
			type: 'string',
			typeOptions: { password: true },
			default: '',
		},
	];

	authenticate: IAuthenticateGeneric = {
		type: 'generic',
		properties: {
			headers: {
				'api_key': '={{$credentials.apiKey}}',
			},
		},
	};
}
