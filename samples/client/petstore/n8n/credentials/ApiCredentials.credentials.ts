import type {
	IAuthenticateGeneric,
	ICredentialType,
	INodeProperties,
} from 'n8n-workflow';

/**
 * Generated from the OpenAPI spec's declared security scheme(s). When more than one
 * scheme is declared, the first one found (priority: apiKey, then HTTP basic, then HTTP
 * bearer, then OAuth2) is wired up automatically here; any additional schemes are listed
 * in the generated README as a manual-setup limitation rather than silently dropped.
 */
export class ApiCredentials implements ICredentialType {
	name = 'apiCredentialsApi';

	displayName = "OpenAPI Petstore API";

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
