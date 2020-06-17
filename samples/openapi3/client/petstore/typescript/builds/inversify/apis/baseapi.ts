import { Configuration } from '../configuration'
import { injectable, inject } from "inversify";
import { AbstractConfiguration } from "../services/configuration";

/**
 *
 * @export
 */
export const COLLECTION_FORMATS = {
    csv: ",",
    ssv: " ",
    tsv: "\t",
    pipes: "|",
};


/**
 * 
 * @export
 * @class BaseAPI
 */
@injectable()
export class BaseAPIRequestFactory {

    constructor(@inject(AbstractConfiguration) protected configuration: Configuration) {
    }
};

/**
 * 
 * @export
 * @class RequiredError
 * @extends {Error}
 */
export class RequiredError extends Error {
    name: "RequiredError" = "RequiredError";
    constructor(public field: string, msg?: string) {
        super(msg);
    }
}
