export * from './advancedApi';
import { AdvancedApi } from './advancedApi';
export * from './basicApi';
import { BasicApi } from './basicApi';
export * from './defaultApi';
import { DefaultApi } from './defaultApi';
import * as http from 'http';

export class HttpError extends Error {
    constructor (public response: http.IncomingMessage, public body: any, public statusCode?: number) {
        super('HTTP request failed');
        this.name = 'HttpError';
    }
}

export { RequestFile } from '../model/models';

export const APIS = [AdvancedApi, BasicApi, DefaultApi];
