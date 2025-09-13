import {provideZonelessChangeDetection} from '@angular/core';
import {TestBed} from '@angular/core/testing'
import {provideHttpClient} from '@angular/common/http'
import {HttpTestingController, provideHttpClientTesting} from '@angular/common/http/testing'
import {DefaultService, Filter, provideApi} from '@swagger/typescript-angular-query-param-json'

describe('JSON Query Param testing', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      providers: [
        provideZonelessChangeDetection(),
        provideHttpClient(),
        provideHttpClientTesting(),
        provideApi('http://localhost'),
      ]
    }).compileComponents();
  });

  it('should serialize the query parameter as URI encoded JSON', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    const filter: Filter = {ids: [4, 5], name: 'John+,=}', age: 37};
    const filterJson = '{"ids":[4,5],"name":"John+,=}","age":37}';
    const filterJsonEncoded = encodeURIComponent(filterJson);

    service.search(filter).subscribe();
    const req = httpTesting.expectOne(`http://localhost/search?filter=${filterJsonEncoded}`);
    expect(req.request.method).toEqual('GET');
  });
});
