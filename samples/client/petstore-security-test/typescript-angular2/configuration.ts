export interface ConfigurationParameters {
	apiKey?: string;
	username?: string;
	password?: string;
	accessToken?: string;
	basePath?: string;
}

export class Configuration {
	apiKey: string;
	username: string;
	password: string;
	accessToken: string;
	basePath: string;


	constructor(configurationParameters: ConfigurationParameters = {}) {
		this.apiKey = configurationParameters.apiKey;
		this.username = configurationParameters.username;
		this.password = configurationParameters.password;
		this.accessToken = configurationParameters.accessToken;
		this.basePath = configurationParameters.basePath;
	}
}
