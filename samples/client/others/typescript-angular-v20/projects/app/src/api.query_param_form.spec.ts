import {provideZonelessChangeDetection} from '@angular/core';
import {TestBed} from '@angular/core/testing'
import {provideHttpClient} from '@angular/common/http'
import {HttpTestingController, provideHttpClientTesting} from '@angular/common/http/testing'
import {DefaultService, Filter, provideApi} from '@swagger/typescript-angular-query-param-form'

const ids: number[] = [4, 5];
const filter: Filter = {name: 'John', age: 37};
const country: string = "Belgium";

describe('Form Query Param testing', () => {
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

  it('should separate the query parameter with ampersands (all set)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode(ids, filter, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_explode?ids=4&ids=5&name=John&age=37&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (all undefined)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode(undefined, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (empty array only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode([], undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (non empty array only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode(ids, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode?ids=4&ids=5');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (object only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode(undefined, filter, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode?name=John&age=37');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (simple only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchExplode(undefined, undefined, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_explode?country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (all set)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    const filter: Filter = {name: 'John,+ ', age: 37};
    service.searchNotExplode(ids, filter, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=4,5&filter=name,John%2C%2B%20,age,37&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (all undefined)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchNotExplode(undefined, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (empty array only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchNotExplode([], undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (non empty array only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchNotExplode(ids, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=4,5');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (object only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchNotExplode(undefined, filter, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?filter=name,John,age,37');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (simple only)', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const service = TestBed.inject(DefaultService);

    service.searchNotExplode(undefined, undefined, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_not_explode?country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

});
