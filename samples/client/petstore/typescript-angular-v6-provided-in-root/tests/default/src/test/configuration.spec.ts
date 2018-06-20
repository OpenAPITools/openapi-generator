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
  Category,
} from '@swagger/typescript-angular-petstore';


describe(`API`, () => {
  const getCategory: () => Category = () => {
    return { }
  };

  const getPet: () => Pet = () => {
    const time = Date.now();
    return {
      name: `pet-${time}`,
      photoUrls: []
    }
  };

  const options = {
    credentials: 'include',
    mode: 'cors'
  };

  const newPet: Pet = getPet();
  let createdPet: Pet;

  const apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    apiKeys: { api_key: "foobar" }
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
          createdPet = result;
          return expect(result.id).toBeGreaterThan(0);
        },
        (error) => {
          return expect(error).toBeFalsy();
        }
      );
    }));

    it(`should have created a pet`, () => {
      expect(createdPet.name).toEqual(newPet.name);
    });

    it(`should get the pet data by id`, async(() => {
      const petService = TestBed.get(PetService);
      return petService.getPetById(createdPet.id).subscribe(
        (result) => {
          return expect(result.name).toEqual(newPet.name);
        },
        (error) => {
          return expect(error).toBeFalsy();
        }
      );
    }));

    it(`should update the pet name by pet object`, async(() => {
      const petService = TestBed.get(PetService);
      const newName = `pet-${Date.now()}`;
      createdPet.name = newName;

      return petService.updatePet(createdPet).subscribe(
        (result) => {
          return expect(result.name).toEqual(newName);
        },
        (error) => {
          return expect(error).toBeFalsy();
        }
      );
    }));

    it(`should delete the pet`, async(() => {
      const petService = TestBed.get(PetService);

      return petService.deletePet(createdPet.id).subscribe(
        (result) => {
          return expect(result).toBeFalsy();
        },
        (error) => {
          return expect(error).toBeFalsy();
        }
      );
    }));

  });

});
