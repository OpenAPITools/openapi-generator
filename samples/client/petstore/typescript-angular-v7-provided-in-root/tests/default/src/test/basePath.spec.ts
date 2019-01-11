import { TestBed, async } from '@angular/core/testing';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController} from '@angular/common/http/testing';
import {
  ApiModule,
  PetService,
  Pet,
} from '@swagger/typescript-angular-petstore';
import {BASE_PATH} from "../../../../builds/default/variables";

describe(`API (basePath)`, () => {
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
        { provide: BASE_PATH, useValue: '//test'}
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

    it(`should call to the injected basePath //test/pet`, async(() => {
      const petService = TestBed.get(PetService);

      petService.addPet(pet).subscribe(
        result => expect(result).toEqual(pet),
        error => fail(`expected a result, not the error: ${error.message}`),
      );

      const req = httpTestingController.expectOne('//test/pet');
      expect(req.request.method).toEqual('POST');
      req.flush(pet);
    }));

  });

});
