export interface ConfigurationParameters {
	apiKeys?: {[ key: string ]: string};
	username?: string;
	password?: string;
	accessToken?: string;
	basePath?: string;
}

export class Configuration {
	apiKeys: {[ key: string ]: string};
	username: string;
	password: string;
	accessToken: string | (() => string);
	basePath: string;


	constructor(configurationParameters: ConfigurationParameters = {}) {
		this.apiKeys = configurationParameters.apiKeys;
		this.username = configurationParameters.username;
		this.password = configurationParameters.password;
		this.accessToken = configurationParameters.accessToken;
		this.basePath = configurationParameters.basePath;
	}
}
