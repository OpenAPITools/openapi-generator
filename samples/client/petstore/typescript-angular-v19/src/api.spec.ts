import { TestBed } from '@angular/core/testing'
import { provideHttpClient } from '@angular/common/http'
import { HttpTestingController, provideHttpClientTesting } from '@angular/common/http/testing'
import { DefaultService } from '@swagger/typescript-angular-deepobject'

describe('DeepObject Query Param testing', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      providers: [
        provideHttpClient(),
        provideHttpClientTesting(),
        DefaultService,
      ],
    }).compileComponents();
  });

  it('should generate the deepObject query with the correct parameters', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);
    const carService = TestBed.inject(DefaultService)
    carService.getCars({ make: 'bmw', model: '319' }).subscribe()
    const req = httpTesting.expectOne('http://localhost/car?filter%5Bmake%5D=bmw&filter%5Bmodel%5D=319')
    expect(req.request.method).toEqual('GET')
  });
});