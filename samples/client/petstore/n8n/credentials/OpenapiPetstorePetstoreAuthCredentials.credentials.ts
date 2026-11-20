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
 */
export class OpenapiPetstorePetstoreAuthCredentials implements ICredentialType {
	name = 'openapiPetstorePetstoreAuthCredentials';

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
			description: 'This spec declares an OAuth2 scheme. Full OAuth2 flow automation is a documented limitation (see README) - paste a token obtained out of band here for now.',
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
