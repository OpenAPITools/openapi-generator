import { TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController} from '@angular/common/http/testing';
import {
  ApiModule,
  Configuration,
  ConfigurationParameters,
  PetService,
  Pet,
} from '@swagger/typescript-angular-petstore';

describe(`API (with ConfigurationFactory)`, () => {
  let httpClient: HttpClient;
  let httpTestingController: HttpTestingController;

  const pet: Pet = {
    name: `pet`,
    photoUrls: []
  };

  let apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    basePath: '//test-initial'
  };

  let apiConfig: Configuration = new Configuration(apiConfigurationParams);

  const getApiConfig = () => {
    return apiConfig;
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule ,
        ApiModule.forRoot(getApiConfig)
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

    it(`should call initially configured basePath //test-initial/pet`, waitForAsync(() => {
      const petService = TestBed.inject(PetService);

      petService.addPet(pet).subscribe(
        result => expect(result).toEqual(pet),
        error => fail(`expected a result, not the error: ${error.message}`),
      );

      const req = httpTestingController.expectOne('//test-initial/pet');

      expect(req.request.method).toEqual('POST');

      req.flush(pet);
    }));
  });

});

describe(`API (with ConfigurationFactory and empty basePath)`, () => {
  let httpClient: HttpClient;
  let httpTestingController: HttpTestingController;

  const pet: Pet = {
    name: `pet`,
    photoUrls: []
  };

  let apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    basePath: ''
  };

  let apiConfig: Configuration = new Configuration(apiConfigurationParams);

  const getApiConfig = () => {
    return apiConfig;
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule ,
        ApiModule.forRoot(getApiConfig)
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

    it(`should call initially configured empty basePath /pet`, waitForAsync(() => {
      const petService = TestBed.inject(PetService);

      petService.addPet(pet).subscribe(
        result => expect(result).toEqual(pet),
        error => fail(`expected a result, not the error: ${error.message}`),
      );

      const req = httpTestingController.expectOne('/pet');

      expect(req.request.method).toEqual('POST');

      req.flush(pet);
    }));
  });

});
