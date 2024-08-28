import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http.ts';
import { Configuration} from '../configuration.ts'

import { ChatMessage } from '../models/ChatMessage.ts';
import { Email } from '../models/Email.ts';
import { Interaction } from '../models/Interaction.ts';
import { ObservableDefaultApi } from './ObservableAPI.ts';

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi.ts";
export class PromiseDefaultApi {
    private api: ObservableDefaultApi

    public constructor(
        configuration: Configuration,
        requestFactory?: DefaultApiRequestFactory,
        responseProcessor?: DefaultApiResponseProcessor
    ) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param parameterTypeMapping 
     */
    public interactionsGetWithHttpInfo(parameterTypeMapping?: string, _options?: Configuration): Promise<HttpInfo<Interaction>> {
        const result = this.api.interactionsGetWithHttpInfo(parameterTypeMapping, _options);
        return result.toPromise();
    }

    /**
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param parameterTypeMapping 
     */
    public interactionsGet(parameterTypeMapping?: string, _options?: Configuration): Promise<Interaction> {
        const result = this.api.interactionsGet(parameterTypeMapping, _options);
        return result.toPromise();
    }


}



