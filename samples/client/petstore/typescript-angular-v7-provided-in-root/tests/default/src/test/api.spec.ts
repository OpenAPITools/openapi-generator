import { TestBed, async } from '@angular/core/testing';
import {HttpClientModule} from '@angular/common/http';
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
import {fakePetstoreBackendProviders} from "./fakeBackend";
import {switchMap} from "rxjs/operators";


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

  const newUser: User = getUser();

  const apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    apiKeys: { api_key: 'foobar' }
  };

  const apiConfig = new Configuration(apiConfigurationParams);

  const getApiConfig = () => {
    return apiConfig;
  };

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
        ...fakePetstoreBackendProviders,
      ]
    });
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
          expect(result.id).toBeGreaterThan(0);
          expect(result.name).toBe(newPet.name);
        },
      );
    }));

    it(`should get the pet data by id`, async(() => {
      const petService = TestBed.get(PetService);
      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.getPetById(addedPet.id))
      ).subscribe(
        result => {
          return expect(result.name).toBe(newPet.name);
        },
      );
    }));

    it(`should update the pet name by pet object`, async(() => {
      const petService = TestBed.get(PetService);


      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.updatePet({
          ...addedPet,
          name: 'something else'
        }))
      ).subscribe(
        result => expect(result.name).toBe('something else'),
        error => fail(`expected a result, not the error: ${error.message}`),
      );
    }));

    it(`should delete the pet`, async(() => {
      const petService = TestBed.get(PetService);

      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.deletePet(addedPet.id, undefined, 'response')),
      ).subscribe(
        result => expect(result.status).toEqual(200),
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
        result => expect(result.mega).toBe(42),
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

      return userService.createUser(newUser, 'response').subscribe(
        result => expect(result.status).toEqual(200),
      );
    }));

  });
});
