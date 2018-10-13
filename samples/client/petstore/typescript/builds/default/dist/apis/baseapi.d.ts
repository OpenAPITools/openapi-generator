import { Configuration } from '../configuration';
export declare const COLLECTION_FORMATS: {
    csv: string;
    ssv: string;
    tsv: string;
    pipes: string;
};
export declare class BaseAPIRequestFactory {
    protected configuration: Configuration;
    constructor(configuration: Configuration);
}
export declare class RequiredError extends Error {
    field: string;
    name: "RequiredError";
    constructor(field: string, msg?: string);
}
