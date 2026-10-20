import {
    DefaultApi,
} from '@openapitools/typescript-node-encode-decode';
import { expect } from 'chai';
import { BASE_URL, encodeMock } from './server';

const api = new DefaultApi(BASE_URL);

describe('deserialization', () => {
    it('deserializes primitive booleans', async () => {
        const { body: response } = await api.testDecodePrimitiveBooleanGet();
        expect(response).to.be.true;
    });

    it('deserializes primitive integers', async () => {
        const { body: response } = await api.testDecodePrimitiveIntegerGet();
        expect(response).to.equal(42);
    });

    it('deserializes primitive numbers', async () => {
        const { body: response } = await api.testDecodePrimitiveNumberGet();
        expect(response).to.equal(42.42);
    });

    it('deserializes primitive strings', async () => {
        const { body: response } = await api.testDecodePrimitiveStringGet();
        expect(response).to.equal('some string value');
    });

    it('deserializes nullable strings', async () => {
        const { body: response } = await api.testDecodeNullableGet();
        expect(response).to.equal(null);
    });

    it('deserializes arrays of strings', async () => {
        const { body: response } = await api.testDecodeArrayOfGet();
        expect(response).to.deep.equal(["first", "second", "third"]);
    });

    it('deserializes arrays of nullable strings', async () => {
        const { body: response } = await api.testDecodeArrayOfNullableGet();
        expect(response).to.deep.equal(["first", null, "third"]);
    });

    it('deserializes nullable arrays', async () => {
        const { body: response } = await api.testDecodeNullableArrayGet();
        expect(response).to.equal(null);
    });

    it('deserializes arrays of arrays', async () => {
        const { body: response } = await api.testDecodeArrayOfArraysGet();
        expect(response).to.deep.equal([["first", "second"], ["third"]]);
    });

    it('deserializes objects', async () => {
        const { body: response } = await api.testDecodeObjectGet();
        expect(response).to.deep.equal({
            requiredProperty: "required",
            optionalProperty: undefined,
            requiredNullableProperty: null,
            optionalNullableProperty: null,
        });
    });

    it('deserializes maps of primitives', async () => {
        const { body: response } = await api.testDecodeMapOfPrimitiveGet();
        expect(response).to.deep.equal({
            key1: "value1",
            key2: "value2",
        });
    });

    it('deserializes maps of objects', async () => {
        const { body: response } = await api.testDecodeMapOfObjectsGet();
        expect(response).to.deep.equal({
            barebones: {
                requiredProperty: "first",
                requiredNullableProperty: null,
                optionalProperty: undefined,
                optionalNullableProperty: undefined,
            },
            nulls: {
                requiredProperty: "second",
                requiredNullableProperty: null,
                optionalProperty: undefined,
                optionalNullableProperty: null,
            },
            values: {
                requiredProperty: "third",
                requiredNullableProperty: "foo",
                optionalProperty: "bar",
                optionalNullableProperty: "baz",
            }
        });
    });

    it('deserializes maps of maps of objects', async () => {
        const { body: response } = await api.testDecodeMapOfMapsOfObjectsGet();
        expect(response).to.deep.equal({
            key1: {
                key1: {
                    requiredProperty: "first",
                    requiredNullableProperty: null,
                    optionalProperty: undefined,
                    optionalNullableProperty: undefined,
                },
                key2: {
                    requiredProperty: "second",
                    requiredNullableProperty: null,
                    optionalProperty: undefined,
                    optionalNullableProperty: null,
                },
            },
            key2: {
                key3: {
                    requiredProperty: "third",
                    requiredNullableProperty: "foo",
                    optionalProperty: "bar",
                    optionalNullableProperty: "baz",
                }
            }
        });
    });

    it('deserializes arrays of maps of objects', async () => {
        const { body: response } = await api.testDecodeArrayOfMapsOfObjectsGet();
        expect(response).to.deep.equal([
            {
                key1: {
                    requiredProperty: "first",
                    requiredNullableProperty: null,
                    optionalProperty: undefined,
                    optionalNullableProperty: undefined,
                },
                key2: {
                    requiredProperty: "second",
                    requiredNullableProperty: null,
                    optionalProperty: undefined,
                    optionalNullableProperty: null,
                },
            },
            {
                key3: {
                    requiredProperty: "third",
                    requiredNullableProperty: "foo",
                    optionalProperty: "bar",
                    optionalNullableProperty: "baz",
                }
            }
        ]);
    });

    it('deserializes arrays of nullable objects', async () => {
        const { body: response } = await api.testDecodeArrayOfNullableObjectsGet();
        expect(response).to.deep.equal([
            {
                requiredProperty: "first",
                requiredNullableProperty: null,
                optionalProperty: undefined,
                optionalNullableProperty: undefined,
            },
            null,
            {
                requiredProperty: "third",
                requiredNullableProperty: "foo",
                optionalProperty: "bar",
                optionalNullableProperty: "baz",
            }
        ]);
    });

    it('deserializes composite objects', async () => {
        const { body: response } = await api.testDecodeCompositeObjectsGet();
        expect(response).to.deep.equal({
            optionalNullableInnerObject: {
                requiredProperty: "required",
                requiredNullableProperty: null,
                optionalProperty: undefined,
                optionalNullableProperty: undefined,
            },
        });
    });
});

describe("serialization", () => {
    it("serializes primitive booleans", async () => {
        await api.testEncodePrimitiveBooleanPost(true);
        expect(encodeMock.lastRequestBody).to.equal(true);
    });

    it("serializes primitive integers", async () => {
        await api.testEncodePrimitiveIntegerPost(42);
        expect(encodeMock.lastRequestBody).to.equal(42);
    });

    it("serializes primitive numbers", async () => {
        await api.testEncodePrimitiveNumberPost(42.42);
        expect(encodeMock.lastRequestBody).to.equal(42.42);
    });

    it("serializes primitive strings", async () => {
        await api.testEncodePrimitiveStringPost("some string value");
        expect(encodeMock.lastRequestBody).to.equal("some string value");
    });

    it("serializes nullable strings", async () => {
        await api.testEncodeNullablePost(null);
        expect(encodeMock.lastRequestBody).to.equal(null);
    });

    it("serializes arrays of strings", async () => {
        await api.testEncodeArrayOfPost(["first", "second", "third"]);
        expect(encodeMock.lastRequestBody).to.deep.equal(["first", "second", "third"]);
    });

    it("serializes arrays of nullable strings", async () => {
        await api.testEncodeArrayOfNullablePost(["first", null, "third"]);
        expect(encodeMock.lastRequestBody).to.deep.equal(["first", null, "third"]);
    });

    it("serializes nullable arrays", async () => {
        await api.testEncodeNullableArrayPost(null);
        expect(encodeMock.lastRequestBody).to.equal(null);
    });

    it("serializes arrays of arrays", async () => {
        await api.testEncodeArrayOfArraysPost([["first", "second"], ["third"]]);
        expect(encodeMock.lastRequestBody).to.deep.equal([["first", "second"], ["third"]]);
    });

    it("serializes objects", async () => {
        await api.testEncodeObjectPost({
            requiredProperty: "required",
            requiredNullableProperty: null,
            optionalNullableProperty: null,
        });
        expect(encodeMock.lastRequestBody).to.deep.equal({
            required_property: "required",
            required_nullable_property: null,
            optional_nullable_property: null,
        });
    });

    it("serializes maps of primitives", async () => {
        await api.testEncodeMapOfPrimitivePost({
            key1: "value1",
            key2: "value2",
        });
        expect(encodeMock.lastRequestBody).to.deep.equal({
            key1: "value1",
            key2: "value2",
        });
    });

    it("serializes maps of objects", async () => {
        await api.testEncodeMapOfObjectsPost({
            barebones: {
                requiredProperty: "first",
                requiredNullableProperty: null,
            },
            nulls: {
                requiredProperty: "second",
                requiredNullableProperty: null,
                optionalNullableProperty: null,
            },
            values: {
                requiredProperty: "third",
                requiredNullableProperty: "foo",
                optionalProperty: "bar",
                optionalNullableProperty: "baz",
            }
        });
        expect(encodeMock.lastRequestBody).to.deep.equal({
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
    });

    it("serializes maps of maps of objects", async () => {
        await api.testEncodeMapOfMapsOfObjectsPost({
            key1: {
                key1: {
                    requiredProperty: "first",
                    requiredNullableProperty: null,
                },
                key2: {
                    requiredProperty: "second",
                    requiredNullableProperty: null,
                    optionalNullableProperty: null,
                },
            },
            key2: {
                key3: {
                    requiredProperty: "third",
                    requiredNullableProperty: "foo",
                    optionalProperty: "bar",
                    optionalNullableProperty: "baz",
                }
            }
        });
        expect(encodeMock.lastRequestBody).to.deep.equal({
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
    });

    it("serializes arrays of maps of objects", async () => {
        await api.testEncodeArrayOfMapsOfObjectsPost([
            {
                key1: {
                    requiredProperty: "first",
                    requiredNullableProperty: null,
                },
                key2: {
                    requiredProperty: "second",
                    requiredNullableProperty: null,
                    optionalNullableProperty: null,
                },
            },
            {
                key3: {
                    requiredProperty: "third",
                    requiredNullableProperty: "foo",
                    optionalProperty: "bar",
                    optionalNullableProperty: "baz",
                }
            }
        ]);
        expect(encodeMock.lastRequestBody).to.deep.equal([
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
    });

    it("serializes arrays of nullable objects", async () => {
        await api.testEncodeArrayOfNullableObjectsPost([
            {
                requiredProperty: "first",
                requiredNullableProperty: null,
            },
            null,
            {
                requiredProperty: "third",
                requiredNullableProperty: "foo",
                optionalProperty: "bar",
                optionalNullableProperty: "baz",
            }
        ]);
        expect(encodeMock.lastRequestBody).to.deep.equal([
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
    });

    it("serializes composite objects", async () => {
        await api.testEncodeCompositeObjectsPost({
            optionalNullableInnerObject: {
                requiredProperty: "required",
                requiredNullableProperty: null,
            },
        });
        expect(encodeMock.lastRequestBody).to.deep.equal({
            optional_nullable_inner_object: {
                required_property: "required",
                required_nullable_property: null,
            },
        });
    });
});
