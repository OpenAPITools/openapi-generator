import { HttpHeaders, HttpParams, HttpParameterCodec } from '@angular/common/http';
import { Param } from './param';

export interface ConfigurationParameters {
    /**
     *  @deprecated Since 5.0. Use credentials instead
     */
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    /**
     *  @deprecated Since 5.0. Use credentials instead
     */
    accessToken?: string | (() => string);
    basePath?: string;
    withCredentials?: boolean;
    /**
     * Takes care of encoding query- and form-parameters.
     */
    encoder?: HttpParameterCodec;
    /**
     * Override the default method for encoding path parameters in various
     * <a href="https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#style-values">styles</a>.
     * <p>
     * See {@link README.md} for more details
     * </p>
     */
    encodeParam?: (param: Param) => string;
    /**
     * The keys are the names in the securitySchemes section of the OpenAPI
     * document. They should map to the value used for authentication
     * minus any standard prefixes such as 'Basic' or 'Bearer'.
     */
    credentials?: {[ key: string ]: string | (() => string | undefined)};
}

export class Configuration {
    /**
     *  @deprecated Since 5.0. Use credentials instead
     */
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    /**
     *  @deprecated Since 5.0. Use credentials instead
     */
    accessToken?: string | (() => string);
    basePath?: string;
    withCredentials?: boolean;
    /**
     * Takes care of encoding query- and form-parameters.
     */
    encoder?: HttpParameterCodec;
    /**
     * Encoding of various path parameter
     * <a href="https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#style-values">styles</a>.
     * <p>
     * See {@link README.md} for more details
     * </p>
     */
    encodeParam: (param: Param) => string;
    /**
     * The keys are the names in the securitySchemes section of the OpenAPI
     * document. They should map to the value used for authentication
     * minus any standard prefixes such as 'Basic' or 'Bearer'.
     */
    credentials: {[ key: string ]: string | (() => string | undefined)};

constructor({ accessToken, apiKeys, basePath, credentials, encodeParam, encoder, password, username, withCredentials }: ConfigurationParameters = {}) {
        if (apiKeys) {
            this.apiKeys = apiKeys;
        }
        if (username !== undefined) {
            this.username = username;
        }
        if (password !== undefined) {
            this.password = password;
        }
        if (accessToken !== undefined) {
            this.accessToken = accessToken;
        }
        if (basePath !== undefined) {
            this.basePath = basePath;
        }
        if (withCredentials !== undefined) {
            this.withCredentials = withCredentials;
        }
        if (encoder) {
            this.encoder = encoder;
        }
        this.encodeParam = encodeParam ?? (param => this.defaultEncodeParam(param));
        this.credentials = credentials ?? {};
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

    public lookupCredential(key: string): string | undefined {
        const value = this.credentials[key];
        return typeof value === 'function'
            ? value()
            : value;
    }

    public addCredentialToHeaders(credentialKey: string, headerName: string, headers: HttpHeaders, prefix?: string): HttpHeaders {
        const value = this.lookupCredential(credentialKey);
        return value
            ? headers.set(headerName, (prefix ?? '') + value)
            : headers;
    }

    public addCredentialToQuery(credentialKey: string, paramName: string, query: HttpParams): HttpParams {
        const value = this.lookupCredential(credentialKey);
        return value
            ? query.set(paramName, value)
            : query;
    }

    private defaultEncodeParam(param: Param): string {
        // This implementation exists as fallback for missing configuration
        // and for backwards compatibility to older typescript-angular generator versions.
        // It only works for the 'simple' parameter style.
        // Date-handling only works for the 'date-time' format.
        // All other styles and Date-formats are probably handled incorrectly.
        //
        // But: if that's all you need (i.e.: the most common use-case): no need for customization!

        const value = param.dataFormat === 'date-time' && param.value instanceof Date
            ? (param.value as Date).toISOString()
            : param.value;

        return encodeURIComponent(String(value));
    }
}
