import { TestBed, async } from '@angular/core/testing';
import { HttpClientModule } from '@angular/common/http';
import {
  ApiModule,
  Configuration,
  ConfigurationParameters,
  PetService,
  StoreService,
  UserService,
  Pet,
  User,
} from '@swagger/typescript-angular-petstore';


describe(`API (functionality)`, () => {

  const getUser: () => User = () => {
    const time = Date.now();
    return {
      username: `user-${time}`,
    }
  };

  const getPet: () => Pet = () => {
    const time = Date.now();
    return {
      name: `pet-${time}`,
      photoUrls: [],
    }
  };

  const newPet: Pet = getPet();
  let createdPet: Pet;

  const newUser: User = getUser();

  const apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    apiKeys: { api_key: "foobar" }
  };

  const apiConfig = new Configuration(apiConfigurationParams);

  const getApiConfig = () => {
    return apiConfig;
  };

  let originalTimeout;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientModule,
        ApiModule.forRoot(getApiConfig)
      ],
      providers: [
        PetService,
        StoreService,
        UserService,
      ]
    });

    originalTimeout = jasmine.DEFAULT_TIMEOUT_INTERVAL;
    jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;
  });

  afterEach(() => {
    jasmine.DEFAULT_TIMEOUT_INTERVAL = originalTimeout;
  });

  describe(`PetService`, () => {
    it(`should be provided`, () => {
      const petService = TestBed.get(PetService);
      expect(petService).toBeTruthy();
    });

    it(`should add a pet`, async(() => {
      const petService = TestBed.get(PetService);

      return petService.addPet(newPet).subscribe(
        (result) => {
          createdPet = result;
          return expect(result.id).toBeGreaterThan(0);
        },
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

    it(`should have created a pet`, () => {
      expect(createdPet.name).toEqual(newPet.name);
    });

    it(`should get the pet data by id`, async(() => {
      const petService = TestBed.get(PetService);
      return petService.getPetById(createdPet.id).subscribe(
        result => expect(result.name).toEqual(newPet.name),
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

    it(`should update the pet name by pet object`, async(() => {
      const petService = TestBed.get(PetService);
      const newName = `pet-${Date.now()}`;
      createdPet.name = newName;

      return petService.updatePet(createdPet).subscribe(
        result => expect(result.name).toEqual(newName),
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

    it(`should delete the pet`, async(() => {
      const petService = TestBed.get(PetService);

      return petService.deletePet(createdPet.id).subscribe(
        result => expect(result.code).toEqual(200),
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

  });

  describe(`StoreService`, () => {
    it(`should be provided`, () => {
      const storeService = TestBed.get(StoreService);
      expect(storeService).toBeTruthy();
    });

    it(`should get the inventory`, async(() => {
      const storeService = TestBed.get(StoreService);

      return storeService.getInventory().subscribe(
        result => expect(result).toBeTruthy(),
        error => fail(`expected a result, not the error: ${error.message}`),
      );

    }));

  });

  describe(`UserService`, () => {
    it(`should be provided`, () => {
      const userService = TestBed.get(UserService);
      expect(userService).toBeTruthy();
    });

    it(`should create the user`, async(() => {
      const userService = TestBed.get(UserService);

      return userService.createUser(newUser).subscribe(
        result => expect(result.code).toEqual(200),
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

  });
});
