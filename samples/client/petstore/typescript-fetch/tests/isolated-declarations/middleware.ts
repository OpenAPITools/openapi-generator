import { BaseAPI, type Middleware } from '../../builds/default/runtime.js';

class CustomAPI extends BaseAPI {
    customOperation(): string {
        return 'custom';
    }
}

const api = new CustomAPI();
const middleware: Middleware = {};

// Each method must preserve the subclass type for chained API calls.
const withMiddleware: CustomAPI = api.withMiddleware(middleware);
const withPreMiddleware: CustomAPI = api.withPreMiddleware(async () => undefined);
const withPostMiddleware: CustomAPI = api.withPostMiddleware(async () => undefined);
withMiddleware.customOperation();
withPreMiddleware.customOperation();
withPostMiddleware.customOperation();
