export * from './petApi';
import { PetApi } from './petApi';
export * from './storeApi';
import { StoreApi } from './storeApi';
export * from './userApi';
import { UserApi } from './userApi';
import http = require('http');
export class HttpError extends Error {
    constructor (public response: http.ClientResponse, public body: any, public statusCode?: number) {
        super('HTTP request failed');
        this.name = 'HttpError';
    }
}
export const APIS = [PetApi, StoreApi, UserApi];
