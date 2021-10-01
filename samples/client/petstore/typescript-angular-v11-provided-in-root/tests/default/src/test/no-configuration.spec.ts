import { TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController} from '@angular/common/http/testing';
import {
  ApiModule,
  PetService,
  Pet,
} from '@swagger/typescript-angular-petstore';

describe(`API (no configuration)`, () => {
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
    httpClient = TestBed.inject(HttpClient);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    // After every test, assert that there are no more pending requests.
    httpTestingController.verify();
  });

  describe(`PetService`, () => {
    it(`should be provided`, () => {
      const petService = TestBed.inject(PetService);
      expect(petService).toBeTruthy();
    });

    it(`should call to the default basePath http://petstore.swagger.io/v2/pet`, waitForAsync(() => {
      const petService = TestBed.inject(PetService);

      petService.addPet(pet).subscribe(
        result => expect(result).toEqual(pet),
        error => fail(`expected a result, not the error: ${error.message}`),
      );

      const req = httpTestingController.expectOne('http://petstore.swagger.io/v2/pet');
      expect(req.request.method).toEqual('POST');
      req.flush(pet);
    }));

  });

});
