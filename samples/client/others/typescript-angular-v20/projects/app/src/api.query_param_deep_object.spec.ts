import {provideZonelessChangeDetection} from '@angular/core';
import {TestBed} from '@angular/core/testing'
import {provideHttpClient} from '@angular/common/http'
import {HttpTestingController, provideHttpClientTesting} from '@angular/common/http/testing'
import {DefaultService} from '@swagger/typescript-angular-query-param-deep-object'
import {provideApi} from '@swagger/typescript-angular-query-param-deep-object';

import {BaseService} from '../../../builds/query-param-deep-object/api.base.service';
import {OpenApiHttpParams, QueryParamStyle} from '../../../builds/query-param-deep-object/query.params';

describe('DeepObject Query Param testing', () => {
  let httpTesting: HttpTestingController;
  let service: DefaultService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        provideZonelessChangeDetection(),
        provideHttpClient(),
        provideHttpClientTesting(),
        provideApi("http://localhost"),
        DefaultService,
      ]
    });

    httpTesting = TestBed.inject(HttpTestingController);
    service = TestBed.inject(DefaultService);
  });

  afterEach(() => {
    // Verify that none of the tests make any extra HTTP requests.
    httpTesting.verify();
  });

  it('should generate the deepObject query with the correct parameters', () => {
    service.getCars({ make: 'bmw', model: '319' }).subscribe();
    const req = httpTesting.expectOne('http://localhost/car?filter%5Bmake%5D=bmw&filter%5Bmodel%5D=319');
    expect(req.request.method).toEqual('GET');
  });
});

// Exercise the generated serializer with shapes not restricted by the sample schema.

class QuerySerializer extends BaseService {
  serialize(value: unknown): string {
    return this.addToHttpParams(new OpenApiHttpParams(), 'filter', value, QueryParamStyle.DeepObject, true).toHttpParams().toString();
  }
}

describe('Nested deepObject serialization', () => {
  const serializer = new QuerySerializer();

  it('preserves nested property names and encodes keys and values', () => {
    expect(serializer.serialize({name: {contains: 'A & B'}, 'a b': {value: '+'}}))
      .toBe('filter%5Bname%5D%5Bcontains%5D=A%20%26%20B&filter%5Ba%20b%5D%5Bvalue%5D=%2B');
  });

  it('indexes arrays of objects, primitive arrays and sets', () => {
    expect(serializer.serialize({sort: [{field: 'name'}, {field: 'age'}], ids: [1, 2], tags: new Set(['a', 'b'])}))
      .toBe('filter%5Bsort%5D%5B0%5D%5Bfield%5D=name&filter%5Bsort%5D%5B1%5D%5Bfield%5D=age&filter%5Bids%5D%5B0%5D=1&filter%5Bids%5D%5B1%5D=2&filter%5Btags%5D%5B0%5D=a&filter%5Btags%5D%5B1%5D=b');
  });

  it('omits nullish values and empty containers, preserving false, zero and empty strings', () => {
    expect(serializer.serialize({missing: null, absent: undefined, nested: {zero: 0, no: false, empty: ''}, array: [], object: {}}))
      .toBe('filter%5Bnested%5D%5Bzero%5D=0&filter%5Bnested%5D%5Bno%5D=false&filter%5Bnested%5D%5Bempty%5D=');
    expect(serializer.serialize(null)).toBe('');
  });

  it('serializes dates as ISO strings at any depth', () => {
    expect(serializer.serialize({range: {start: new Date('2026-01-02T03:04:05Z')}}))
      .toBe('filter%5Brange%5D%5Bstart%5D=2026-01-02T03%3A04%3A05.000Z');
  });

  it('still rejects a primitive deepObject parameter', () => {
    expect(() => serializer.serialize('invalid')).toThrowError('An object must be provided for key filter as it is a deep object');
  });
});
