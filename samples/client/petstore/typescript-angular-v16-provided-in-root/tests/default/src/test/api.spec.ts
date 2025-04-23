import { TestBed, waitForAsync } from '@angular/core/testing'
import { HttpClientModule, HttpClient } from '@angular/common/http'
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing'
import {
  ApiModule,
  Configuration,
  type ConfigurationParameters,
  PetService,
  StoreService,
  UserService,
  type Pet,
  type User
} from '@swagger/typescript-angular-petstore'
import { fakePetstoreBackendProviders } from './fakeBackend'
import { switchMap } from 'rxjs/operators'
import { AppComponent } from '../app/app.component'
import { DefaultService } from '@swagger/typescript-angular-deepobject'

describe('API (functionality)', () => {
  const getUser: () => User = () => {
    const time = Date.now()
    return {
      username: `user-${time}`
    }
  }

  const getPet: () => Pet = () => {
    const time = Date.now()
    return {
      name: `pet-${time}`,
      photoUrls: []
    }
  }

  const newPet: Pet = getPet()

  const newUser: User = getUser()

  const apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    apiKeys: { api_key: 'foobar' }
  }

  const apiConfig = new Configuration(apiConfigurationParams)

  function getApiConfig (): Configuration {
    return apiConfig
  }

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
        ...fakePetstoreBackendProviders
      ]
    })
  })

  describe('PetService', () => {
    it('should be provided', () => {
      const petService = TestBed.inject(PetService)
      expect(petService).toBeTruthy()
    })

    it('should add a pet', waitForAsync(() => {
      const petService = TestBed.inject(PetService)

      return petService.addPet(newPet).subscribe(
        (result) => {
          expect(result.id).toBeGreaterThan(0)
          expect(result.name).toBe(newPet.name)
        }
      )
    }))

    it('should get the pet data by id', waitForAsync(() => {
      const petService = TestBed.inject(PetService)
      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.getPetById(addedPet.id))
      ).subscribe(
        result => {
          expect(result.name).toBe(newPet.name)
        }
      )
    }))

    it('should update the pet name by pet object', waitForAsync(() => {
      const petService = TestBed.inject(PetService)

      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.updatePet({
          ...addedPet,
          name: 'something else'
        }))
      ).subscribe(
        result => { expect(result.name).toBe('something else') },
        error => { fail(`expected a result, not the error: ${error.message}`) }
      )
    }))

    it('should delete the pet', waitForAsync(() => {
      const petService = TestBed.inject(PetService)

      return petService.addPet(newPet).pipe(
        switchMap((addedPet: Pet) => petService.deletePet(addedPet.id, undefined, 'response'))
      ).subscribe(
        result => { expect(result.status).toEqual(200) }
      )
    }))
  })

  describe('StoreService', () => {
    it('should be provided', () => {
      const storeService = TestBed.inject(StoreService)
      expect(storeService).toBeTruthy()
    })

    it('should get the inventory', waitForAsync(() => {
      const storeService = TestBed.inject(StoreService)

      return storeService.getInventory().subscribe(
        result => { expect(result.mega).toBe(42) },
        error => { fail(`expected a result, not the error: ${error.message}`) }
      )
    }))
  })

  describe('UserService', () => {
    it('should be provided', () => {
      const userService = TestBed.inject(UserService)
      expect(userService).toBeTruthy()
    })

    it('should create the user', waitForAsync(() => {
      const userService = TestBed.inject(UserService)

      return userService.createUser(newUser, 'response').subscribe(
        result => { expect(result.status).toEqual(200) }
      )
    }))
  })
})


describe('DeepObject Query Param testing', () => {
  let httpClient: HttpClient
  let httpTestingController: HttpTestingController

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ HttpClientTestingModule ]
    })

    // Inject the http service and test controller for each test
    httpClient = TestBed.inject(HttpClient)
    httpTestingController = TestBed.inject(HttpTestingController)
  })
  
  it('should generate the deepObject query with the correct parameters', () => {
    const carService = TestBed.inject(DefaultService)
    carService.getCars({ make: 'bmw', model: '319' }).subscribe()
    const req = httpTestingController.expectOne('http://localhost/car?filter%5Bmake%5D=bmw&filter%5Bmodel%5D=319')
    expect(req.request.method).toEqual('GET')
  })
})