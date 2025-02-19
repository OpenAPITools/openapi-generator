import {
    QueryApi,
    Pet,
    StringEnumRef,
    TestEnumRefStringEnumNonrefStringQueryEnum,
    TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
} from '@openapitools/typescript-axios-echo-api';
import { expect } from 'chai';

describe('QueryApi', () => {
    const api = new QueryApi();

    it('testEnumRefString', async () => {
        const enumNonrefStringQuery = TestEnumRefStringEnumNonrefStringQueryEnum.Success;
        const enumRefStringQuery = StringEnumRef.Unclassified;

        const { request } = await api.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery);
        expect(request.path).to.equal('/query/enum_ref_string?enum_nonref_string_query=success&enum_ref_string_query=unclassified');
    });

    it('testQueryDatetimeDateString', async () => {
        const dateTime = new Date('2023-10-30T10:11:12.000Z');
        const datetimeQuery = dateTime.toISOString();
        const dateQuery = `${dateTime.getFullYear()}-${dateTime.getMonth() + 1}-${dateTime.getDate()}`;
        const stringQuery = 'Hello World';

        const { request } = await api.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery);
        expect(request.path).to.equal('/query/datetime/date/string?datetime_query=2023-10-30T10%3A11%3A12.000Z&date_query=2023-10-30&string_query=Hello+World');
    });

    it('testQueryIntegerBooleanString', async () => {
        const integerQuery = 12345;
        const booleanQuery = true;
        const stringQuery = 'Hello World';

        const { request } = await api.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);
        expect(request.path).to.equal('/query/integer/boolean/string?integer_query=12345&boolean_query=true&string_query=Hello+World');
    });

    it('testQueryStyleFormExplodeTrueArrayString', async () => {
        const arrayStringQuery: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter = {
            values: ['hello world 1', 'hello world 2']
        };

        const { request } = await api.testQueryStyleFormExplodeTrueArrayString(arrayStringQuery);
        expect(request.path).to.equal('/query/style_form/explode_true/array_string?values=hello+world+1&values=hello+world+2');
    });

    it('testQueryStyleFormExplodeTrueObject', async () => {
        const queryObject: Pet = {
            id: 12345,
            name: 'Hello World',
            photoUrls: ['http://a.com', 'http://b.com']
        };

        const { request } = await api.testQueryStyleFormExplodeTrueObject(queryObject);
        expect(request.path).to.equal('/query/style_form/explode_true/object?id=12345&name=Hello+World&photoUrls=http%3A%2F%2Fa.com&photoUrls=http%3A%2F%2Fb.com');
    });

});
1
