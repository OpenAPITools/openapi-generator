export * from './petApi';
import { PetApi } from './petApi';
export * from './storeApi';
import { StoreApi } from './storeApi';
export * from './userApi';
import { UserApi } from './userApi';
import * as http from 'http';

export class HttpError extends Error {
    constructor (public response: http.IncomingMessage, public body: any, public statusCode?: number) {
        super('HTTP request failed');
        this.name = 'HttpError';
    }
}

export { RequestFile } from '../model/models';

export const APIS = [PetApi, StoreApi, UserApi];
