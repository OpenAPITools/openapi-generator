import {provideZonelessChangeDetection} from '@angular/core';
import {TestBed} from '@angular/core/testing'
import {provideHttpClient} from '@angular/common/http'
import {HttpTestingController, provideHttpClientTesting} from '@angular/common/http/testing'
import {DefaultService} from '@swagger/typescript-angular-query-param-deep-object'
import {provideApi} from '@swagger/typescript-angular-query-param-form';

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
