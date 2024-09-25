import {
    createConfiguration,
    PromiseHttpLibrary,
    RequestContext,
    ResponseBody,
    ResponseContext,
    ServerConfiguration,
    wrapHttpLibrary,
} from '@openapitools/typescript-encode-decode';

const BASE_URL = 'http://localhost:1234';

class TestServerStub implements PromiseHttpLibrary {
    public lastRequestBody: any = null;

    async send(request: RequestContext): Promise<ResponseContext> {
        const url = request.getUrl();
        if (!url.startsWith(BASE_URL)) {
            throw new Error(`unexpected url: ${url}`);
        }

        const path = url.substring(BASE_URL.length);

        if (path.startsWith('/test/decode')) {
            const value = this.valueForPath(path);
            const body: ResponseBody = {
                binary: async () => { throw new Error('not implemented') },
                text: async () => JSON.stringify(value),
            };
            const headers = { 'content-type': 'application/json' };
            return new ResponseContext(200, headers, body);
        } else if (path.startsWith('/test/encode/')) {
            const rawBody = request.getBody().toString();
            this.lastRequestBody = JSON.parse(rawBody);
            const body: ResponseBody = {
                binary: async () => { throw new Error('not implemented') },
                text: async () => "",
            };
            return new ResponseContext(200, {}, body);
        } else {
            throw new Error(`unexpected path: ${path}`);
        }
    }

    private valueForPath(path: string): any {
        if (path.startsWith('/test/encode')) {
            return "";
        }

        switch (path) {
            case '/test/decode/primitive/boolean':
                return true;
            case '/test/decode/primitive/integer':
                return 42;
            case '/test/decode/primitive/number':
                return 42.42;
            case '/test/decode/primitive/string':
                return 'some string value';
            case '/test/decode/nullable':
                return null;
            case '/test/decode/array-of':
                return ["first", "second", "third"];
            case '/test/decode/array-of/nullable':
                return ["first", null, "third"];
            case '/test/decode/nullable-array':
                return null;
            case '/test/decode/array-of-arrays':
                return [["first", "second"], ["third"]];
            case '/test/decode/object':
                return {
                    required_property: "required",
                    required_nullable_property: null,
                    optional_nullable_property: null,
                };
            case '/test/decode/map-of/primitive':
                return {
                    key1: "value1",
                    key2: "value2",
                };
            case '/test/decode/map-of/objects':
                return {
                    barebones: {
                        required_property: "first",
                        required_nullable_property: null,
                    },
                    nulls: {
                        required_property: "second",
                        required_nullable_property: null,
                        optional_nullable_property: null,
                    },
                    values: {
                        required_property: "third",
                        required_nullable_property: "foo",
                        optional_property: "bar",
                        optional_nullable_property: "baz",
                    }
                };
            case '/test/decode/map-of/maps-of/objects':
                return {
                    key1: {
                        key1: {
                            required_property: "first",
                            required_nullable_property: null,
                        },
                        key2: {
                            required_property: "second",
                            required_nullable_property: null,
                            optional_nullable_property: null,
                        },
                    },
                    key2: {
                        key3: {
                            required_property: "third",
                            required_nullable_property: "foo",
                            optional_property: "bar",
                            optional_nullable_property: "baz",
                        }
                    }
                };
            case '/test/decode/array-of/maps-of/objects':
                return [
                    {
                        key1: {
                            required_property: "first",
                            required_nullable_property: null,
                        },
                        key2: {
                            required_property: "second",
                            required_nullable_property: null,
                            optional_nullable_property: null,
                        },
                    },
                    {
                        key3: {
                            required_property: "third",
                            required_nullable_property: "foo",
                            optional_property: "bar",
                            optional_nullable_property: "baz",
                        }
                    }
                ];
            case '/test/decode/composite-objects':
                return {
                    optional_nullable_inner_object: {
                        required_property: "required",
                        required_nullable_property: null,
                    },
                };
            case '/test/decode/array-of/nullable-objects':
                return [
                    {
                        required_property: "first",
                        required_nullable_property: null,
                    },
                    null,
                    {
                        required_property: "third",
                        required_nullable_property: "foo",
                        optional_property: "bar",
                        optional_nullable_property: "baz",
                    }
                ];
            default:
                throw new Error(`unexpected path: ${path}`);
        }
    }
}

export const mockServer = new TestServerStub();

export const apiConfiguration = createConfiguration({
    baseServer: new ServerConfiguration(BASE_URL, {}),
    httpApi: wrapHttpLibrary(mockServer),
});
