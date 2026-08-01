import { HttpParams, HttpParameterCodec } from '@angular/common/http';
import { CustomHttpParameterCodec, IdentityHttpParameterCodec } from './encoder';

export enum QueryParamStyle {
    Json,
    Form,
    DeepObject,
    SpaceDelimited,
    PipeDelimited,
}

export type Delimiter = "," | " " | "|" | "\t";

export interface ParamOptions {
    /** When true, serialized as multiple repeated key=value pairs. When false, serialized as a single key with joined values using `delimiter`. */
    explode?: boolean;
    /** Delimiter used when explode=false. The delimiter itself is inserted unencoded between encoded values. */
    delimiter?: Delimiter;
}

interface ParamEntry {
    values: string[];
    options: Required<ParamOptions>;
}

export class OpenApiHttpParams {
    private params: Map<string, ParamEntry> = new Map();
    private defaults: Required<ParamOptions>;
    private encoder: HttpParameterCodec;

    /**
     * @param encoder  Parameter serializer
     * @param defaults Global defaults used when a specific parameter has no explicit options.
     *                 By OpenAPI default, explode is true for query params with style=form.
     */
    constructor(encoder?: HttpParameterCodec, defaults?: { explode?: boolean; delimiter?: Delimiter }) {
        this.encoder = encoder || new CustomHttpParameterCodec();
        this.defaults = {
            explode: defaults?.explode ?? true,
            delimiter: defaults?.delimiter ?? ",",
        };
    }

    private resolveOptions(local?: ParamOptions): Required<ParamOptions> {
        return {
            explode: local?.explode ?? this.defaults.explode,
            delimiter: local?.delimiter ?? this.defaults.delimiter,
        };
    }

    /**
     * Replace the parameter's values and (optionally) its options.
     * Options are stored per-parameter (not global).
     */
    set(key: string, values: string[] | string, options?: ParamOptions): this {
        const arr = Array.isArray(values) ? values.slice() : [values];
        const opts = this.resolveOptions(options);
        this.params.set(key, {values: arr, options: opts});
        return this;
    }

    /**
     * Append a single value to the parameter. If the parameter didn't exist it will be created
     * and use resolved options (global defaults merged with any provided options).
     */
    append(key: string, value: string, options?: ParamOptions): this {
        const entry = this.params.get(key);
        if (entry) {
            // If new options provided, override the stored options for subsequent serialization
            if (options) {
                entry.options = this.resolveOptions({...entry.options, ...options});
            }
            entry.values.push(value);
        } else {
            this.set(key, [value], options);
        }
        return this;
    }

    /**
     * Serialize to a query string according to per-parameter OpenAPI options.
     * - If explode=true for that parameter → repeated key=value pairs (each value encoded).
     * - If explode=false for that parameter → single key=value where values are individually encoded
     *   and joined using the configured delimiter. The delimiter character is inserted AS-IS
     *   (not percent-encoded).
     */
    toString(): string {
        const records = this.toRecord();
        const parts: string[] = [];

        for (const key in records) {
            parts.push(`${key}=${records[key]}`);
        }

        return parts.join("&");
    }

    /**
     * Return parameters as a plain record.
     * - If a parameter has exactly one value, returns that value directly.
     * - If a parameter has multiple values, returns a readonly array of values.
     */
    toRecord(): Record<string, string | number | boolean | ReadonlyArray<string | number | boolean>> {
        const parts: Record<string, string | number | boolean | ReadonlyArray<string | number | boolean>> = {};

        for (const [key, entry] of this.params.entries()) {
            const encodedKey = this.encoder.encodeKey(key);

            if (entry.options.explode) {
                parts[encodedKey] = entry.values.map((v) => this.encoder.encodeValue(v));
            } else {
                const encodedValues = entry.values.map((v) => this.encoder.encodeValue(v));

                // join with the delimiter *unencoded*
                parts[encodedKey] = encodedValues.join(entry.options.delimiter);
            }
        }

        return parts;
    }

    /**
     * Return an Angular's HttpParams with an identity parameter codec as the parameters are already encoded.
     */
    toHttpParams(): HttpParams {
        const records = this.toRecord();

        let httpParams = new HttpParams({encoder: new IdentityHttpParameterCodec()});

        return httpParams.appendAll(records);
    }
}

export function concatHttpParamsObject(httpParams: OpenApiHttpParams, key: string, item: {
    [index: string]: any
}, delimiter: Delimiter): OpenApiHttpParams {
    let keyAndValues: string[] = [];

    for (const k in item) {
        keyAndValues.push(k);

        const value = item[k];

        if (Array.isArray(value)) {
            keyAndValues.push(...value.map(convertToString));
        } else {
            keyAndValues.push(convertToString(value));
        }
    }

    return httpParams.set(key, keyAndValues, {explode: false, delimiter: delimiter});
}

function convertToString(value: any): string {
    if (value instanceof Date) {
         return value.toISOString();
    } else {
        return value.toString();
    }
}
