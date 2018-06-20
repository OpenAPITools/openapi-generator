import { TestBed, async } from '@angular/core/testing';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController} from '@angular/common/http/testing';
import {
  ApiModule,
  Configuration,
  ConfigurationParameters,
  PetService,
  Pet,
} from '@swagger/typescript-angular-petstore';

describe(`Configuration`, () => {
  let httpClient: HttpClient;
  let httpTestingController: HttpTestingController;

  const pet: Pet = {
    name: `pet`,
    photoUrls: []
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule ,
        ApiModule,
      ],
      providers: [
        PetService,
      ]
    });

    // Inject the http service and test controller for each test
    httpClient = TestBed.get(HttpClient);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  afterEach(() => {
    // After every test, assert that there are no more pending requests.
    httpTestingController.verify();
  });

  describe(`PetService`, () => {
    it(`should be provided`, () => {
      const petService = TestBed.get(PetService);
      expect(petService).toBeTruthy();
    });

    it(`should call to the default basePath http://petstore.swagger.io/v2/pet`, async(() => {
      const petService = TestBed.get(PetService);

      petService.addPet(pet).subscribe(
        (result) => {
          return expect(result).toEqual(pet);
        },
        (error) => {
          return fail(`expected a result, not the error: ${error.message}`);
        }
      );

      // The following `expectOne()` will match the request's URL.
      // If no requests or multiple requests matched that URL
      // `expectOne()` would throw.
      const req = httpTestingController.expectOne('http://petstore.swagger.io/v2/pet');

      // Assert that the request is a GET.
      expect(req.request.method).toEqual('POST');

      // Respond with mock data, causing Observable to resolve.
      // Subscribe callback asserts that correct data was returned.
      req.flush(pet);
    }));

  });

});
