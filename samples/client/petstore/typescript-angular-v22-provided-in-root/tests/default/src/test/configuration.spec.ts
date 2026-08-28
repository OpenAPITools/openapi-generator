import { TestBed, waitForAsync } from '@angular/core/testing'
import { HttpTestingController, provideHttpClientTesting } from '@angular/common/http/testing'
import {
  PetService,
  type Pet,
  provideApi
} from '@swagger/typescript-angular-petstore'

describe('API (with ConfigurationFactory)', () => {
  let httpTestingController: HttpTestingController

  const pet: Pet = {
    name: 'pet',
    photoUrls: []
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        PetService,
        provideHttpClientTesting(),
        provideApi({
          basePath: '/test-initial'
        })
      ]
    })

    // Inject the http service and test controller for each test
    httpTestingController = TestBed.inject(HttpTestingController)
  })

  afterEach(() => {
    // After every test, assert that there are no more pending requests.
    httpTestingController.verify()
  })

  describe('PetService', () => {
    it('should be provided', () => {
      const petService = TestBed.inject(PetService)
      expect(petService).toBeTruthy()
    })

    it('should call initially configured basePath /test-initial/pet', waitForAsync(() => {
      const petService = TestBed.inject(PetService)

      petService.addPet(pet).subscribe({
        next: (result: unknown) => expect(result).toEqual(pet),
        error: (error) => fail(`expected a result, not the error: ${error.message}`)
      })

      const req = httpTestingController.expectOne('/test-initial/pet')

      expect(req.request.method).toEqual('POST')

      req.flush(pet)
    }))
  })
})

describe('API (with ConfigurationFactory and empty basePath)', () => {
  let httpTestingController: HttpTestingController

  const pet: Pet = {
    name: 'pet',
    photoUrls: []
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        PetService,
        provideHttpClientTesting(),
        provideApi({
          basePath: ''
        })
      ]
    })

    // Inject the http service and test controller for each test
    httpTestingController = TestBed.inject(HttpTestingController)
  })

  afterEach(() => {
    // After every test, assert that there are no more pending requests.
    httpTestingController.verify()
  })

  describe('PetService', () => {
    it('should be provided', () => {
      const petService = TestBed.inject(PetService)
      expect(petService).toBeTruthy()
    })

    it('should call initially configured empty basePath /pet', waitForAsync(() => {
      const petService = TestBed.inject(PetService)

      petService.addPet(pet).subscribe({
        next: (result: unknown) => expect(result).toEqual(pet),
        error: (error) => fail(`expected a result, not the error: ${error.message}`)
      })

      const req = httpTestingController.expectOne('/pet')

      expect(req.request.method).toEqual('POST')

      req.flush(pet)
    }))
  })
})
