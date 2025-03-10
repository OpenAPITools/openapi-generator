import nock from 'nock';

export const BASE_URL = 'http://localhost:1234';

// Prevent any unmocked requests from making actual HTTP requests.
nock.disableNetConnect();

function mockPath(path: string, value: any) {
    const body = JSON.stringify(value);
    const headers = { 'content-type': 'application/json' };
    nock(BASE_URL).get(path).reply(200, body, headers).persist();
}

mockPath('/test/decode/primitive/boolean', true);
mockPath('/test/decode/primitive/integer', 42);
mockPath('/test/decode/primitive/number', 42.42);
mockPath('/test/decode/primitive/string', 'some string value');
mockPath('/test/decode/nullable', null);
mockPath('/test/decode/array-of', ["first", "second", "third"]);
mockPath('/test/decode/array-of/nullable', ["first", null, "third"]);
mockPath('/test/decode/nullable-array', null);
mockPath('/test/decode/array-of-arrays', [["first", "second"], ["third"]]);
mockPath('/test/decode/object', {
    required_property: "required",
    required_nullable_property: null,
    optional_nullable_property: null,
});
mockPath('/test/decode/map-of/primitive', {
    key1: "value1",
    key2: "value2",
});
mockPath('/test/decode/map-of/objects', {
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
});
mockPath('/test/decode/map-of/maps-of/objects', {
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
});
mockPath('/test/decode/array-of/maps-of/objects', [
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
]);
mockPath('/test/decode/composite-objects', {
    optional_nullable_inner_object: {
        required_property: "required",
        required_nullable_property: null,
    },
});
mockPath('/test/decode/array-of/nullable-objects', [
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
]);

export const encodeMock = {
    lastRequestBody: "",
};

nock(BASE_URL)
    .filteringPath(/^\/test\/encode\/.*/, '/test/encode')
    .post('/test/encode')
    .reply(200, (uri: string, requestBody: string) => {
        encodeMock.lastRequestBody = requestBody;
        return "";
    })
    .persist();

