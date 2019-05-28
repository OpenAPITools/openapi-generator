export interface ConfigurationParameters {
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    accessToken?: string | (() => string);
    basePath?: string;
    withCredentials?: boolean;
}

export class Configuration {
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    accessToken?: string | (() => string);
    basePath?: string;
    withCredentials?: boolean;

    constructor(configurationParameters: ConfigurationParameters = {}) {
        this.apiKeys = configurationParameters.apiKeys;
        this.username = configurationParameters.username;
        this.password = configurationParameters.password;
        this.accessToken = configurationParameters.accessToken;
        this.basePath = configurationParameters.basePath;
        this.withCredentials = configurationParameters.withCredentials;
    }

    /**
     * Select the correct content-type to use for a request.
     * Uses {@link Configuration#isJsonMime} to determine the correct content-type.
     * If no content type is found return the first found type if the contentTypes is not empty
     * @param contentTypes - the array of content types that are available for selection
     * @returns the selected content-type or <code>undefined</code> if no selection could be made.
     */
    public selectHeaderContentType (contentTypes: string[]): string | undefined {
        if (contentTypes.length === 0) {
            return undefined;
        }

        const type = contentTypes.find((x: string) => this.isJsonMime(x));
        if (type === undefined) {
            return contentTypes[0];
        }
        return type;
    }

    /**
     * Select the correct accept content-type to use for a request.
     * Uses {@link Configuration#isJsonMime} to determine the correct accept content-type.
     * If no content type is found return the first found type if the contentTypes is not empty
     * @param accepts - the array of content types that are available for selection.
     * @returns the selected content-type or <code>undefined</code> if no selection could be made.
     */
    public selectHeaderAccept(accepts: string[]): string | undefined {
        if (accepts.length === 0) {
            return undefined;
        }

        const type = accepts.find((x: string) => this.isJsonMime(x));
        if (type === undefined) {
            return accepts[0];
        }
        return type;
    }

    /**
    * Select the response type to use for httpclient library.
    * Uses {@link Configuration#isJsonMime} to determine the correct accept content-type.
    * If no content type is found set to json as default
    * @param accept - the content types set as accepted in the request.
    * @returns the selected content-type.
    */
    public selectResponseType(accept: string): 'arraybuffer' | 'blob' | 'json' | 'text' {
        if (this.isTextMime(accept)) {
            return 'text';
        }
        if (this.isBinaryMime(accept)) {
            return 'blob';
        }
        return 'json';
    }

    /**
     * Check if the given MIME is a JSON MIME.
     * JSON MIME examples:
     *   application/json
     *   application/json; charset=UTF8
     *   APPLICATION/JSON
     *   application/vnd.company+json
     * @param mime - MIME (Multipurpose Internet Mail Extensions)
     * @return True if the given MIME is JSON, false otherwise.
     */
    public isJsonMime(mime: string): boolean {
        const jsonMime: RegExp = new RegExp('^(application\/json|[^;/ \t]+\/[^;/ \t]+[+]json)[ \t]*(;.*)?$', 'i');
        return mime !== null && (jsonMime.test(mime) || mime.toLowerCase() === 'application/json-patch+json');
    }

    /**
    * Check if the given MIME is a text MIME.
    * text MIME examples:
    *   text/plain
    *   text/html
    * @param mime - MIME (Multipurpose Internet Mail Extensions)
    * @return True if the given MIME is text, false otherwise.
    */
    public isTextMime(mime: string): boolean {
        return mime.toLowerCase().startsWith('text/');
    }

    /**
    * Check if the given MIME is a binary MIME.
    * binary MIME examples:
    *   application/octet-stream
    * @param mime - MIME (Multipurpose Internet Mail Extensions)
    * @return True if the given MIME is binary, false otherwise.
    */
    public isBinaryMime(mime: string): boolean {
        return mime.toLowerCase() === 'application/octet-stream';
    }
}
