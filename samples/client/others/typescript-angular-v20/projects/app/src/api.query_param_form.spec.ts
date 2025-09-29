import {provideZonelessChangeDetection} from '@angular/core';
import {TestBed} from '@angular/core/testing'
import {HttpClient, provideHttpClient} from '@angular/common/http'
import {HttpTestingController, provideHttpClientTesting} from '@angular/common/http/testing'
import {DefaultService, Filter, provideApi} from '@swagger/typescript-angular-query-param-form'

const ids: number[] = [4, 5];
const filter: Filter = {name: 'John', age: 37, nicknames: ['Joe', 'Joey']};
const filterWithSpecialCharacters: Filter = {name: 'Éléonore &,|+', age: 42, nicknames: ['Elé', 'Ellie']};
const country: string = "Belgium";

describe('Form Query Param testing', () => {
  let httpTesting: HttpTestingController;
  let service: DefaultService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        provideZonelessChangeDetection(),
        provideHttpClient(),
        provideHttpClientTesting(),
        provideApi('http://localhost'),
        DefaultService,
      ]
    });

    httpTesting = TestBed.inject(HttpTestingController);
    const httpClient = TestBed.inject(HttpClient);
    service = new DefaultService(httpClient, "http://localhost");
  });

  afterEach(() => {
    // Verify that none of the tests make any extra HTTP requests.
    httpTesting.verify();
  });

  it('should separate the query parameter with ampersands (all set)', async () => {
    service.searchExplode(ids, filter, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_explode?ids=4&ids=5&name=John&age=37&nicknames=Joe&nicknames=Joey&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (all set) - special characters', async () => {
    service.searchExplode(ids, filterWithSpecialCharacters, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_explode?ids=4&ids=5&name=%C3%89l%C3%A9onore%20%26%2C%7C%2B&age=42&nicknames=El%C3%A9&nicknames=Ellie&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (all undefined)', async () => {
    service.searchExplode(undefined, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (empty array only)', async () => {
    service.searchExplode([], undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (non empty array only)', async () => {
    service.searchExplode(ids, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode?ids=4&ids=5');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (object only)', async () => {
    service.searchExplode(undefined, filter, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_explode?name=John&age=37&nicknames=Joe&nicknames=Joey');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with ampersands (simple only)', async () => {
    service.searchExplode(undefined, undefined, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_explode?country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (all set)', async () => {
    service.searchNotExplode(ids, filter, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=4,5&filter=name,John,age,37,nicknames,Joe,Joey&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (all set) - special characters', async () => {
    service.searchNotExplode(ids, filterWithSpecialCharacters, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=4,5&filter=name,%C3%89l%C3%A9onore%20%26%2C%7C%2B,age,42,nicknames,El%C3%A9,Ellie&country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (all undefined)', async () => {
    service.searchNotExplode(undefined, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (empty array only)', async () => {
    service.searchNotExplode([], undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (non empty array only)', async () => {
    service.searchNotExplode(ids, undefined, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?ids=4,5');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (object only)', async () => {
    service.searchNotExplode(undefined, filter, undefined).subscribe();

    const req = httpTesting.expectOne('http://localhost/search_not_explode?filter=name,John,age,37,nicknames,Joe,Joey');
    expect(req.request.method).toEqual('GET');
  });

  it('should separate the query parameter with comma (simple only)', async () => {
    service.searchNotExplode(undefined, undefined, country).subscribe();
    const req = httpTesting.expectOne('http://localhost/search_not_explode?country=Belgium');
    expect(req.request.method).toEqual('GET');
  });

});
