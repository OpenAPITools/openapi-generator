import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http.ts';
import { Configuration} from '../configuration.ts'

import { ChatMessage } from '../models/ChatMessage.ts';
import { Email } from '../models/Email.ts';
import { Interaction } from '../models/Interaction.ts';

import { ObservableDefaultApi } from "./ObservableAPI.ts";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi.ts";

export interface DefaultApiInteractionsGetRequest {
    /**
     * 
     * @type string
     * @memberof DefaultApiinteractionsGet
     */
    parameterTypeMapping?: string
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param param the request object
     */
    public interactionsGetWithHttpInfo(param: DefaultApiInteractionsGetRequest = {}, options?: Configuration): Promise<HttpInfo<Interaction>> {
        return this.api.interactionsGetWithHttpInfo(param.parameterTypeMapping,  options).toPromise();
    }

    /**
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param param the request object
     */
    public interactionsGet(param: DefaultApiInteractionsGetRequest = {}, options?: Configuration): Promise<Interaction> {
        return this.api.interactionsGet(param.parameterTypeMapping,  options).toPromise();
    }

}
