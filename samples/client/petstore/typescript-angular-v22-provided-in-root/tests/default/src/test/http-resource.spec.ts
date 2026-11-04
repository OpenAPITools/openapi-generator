import { Injector, signal } from '@angular/core'
import { TestBed } from '@angular/core/testing'
import { HttpContext, HttpContextToken, provideHttpClient } from '@angular/common/http'
import { HttpTestingController, provideHttpClientTesting } from '@angular/common/http/testing'
import {
  Configuration,
  PetService,
  StoreService,
  type Pet
} from '@swagger/typescript-angular-http-resource'

const TOKEN = new HttpContextToken<string>(() => 'none')

describe('API (httpResource)', () => {
  let http: HttpTestingController
  let petService: PetService
  let storeService: StoreService
  const accessToken = signal('first-token')

  // lets the resources react to signal changes and deliver the flushed responses
  const settle = async (): Promise<void> => {
    TestBed.tick()
    await new Promise(resolve => setTimeout(resolve))
    TestBed.tick()
  }

  beforeEach(() => {
    accessToken.set('first-token')
    TestBed.configureTestingModule({
      providers: [
        provideHttpClient(),
        provideHttpClientTesting(),
        {
          provide: Configuration,
          useValue: new Configuration({
            basePath: 'http://localhost/api',
            withCredentials: true,
            credentials: {
              api_key: 'secret-key',
              api_key_query: 'query-key',
              bearer_auth: () => accessToken()
            }
          })
        }
      ]
    })
    http = TestBed.inject(HttpTestingController)
    petService = TestBed.inject(PetService)
    storeService = TestBed.inject(StoreService)
  })

  afterEach(() => {
    http.verify()
  })

  it('stays idle while the parameters are undefined', async () => {
    const petId = signal<number | undefined>(undefined)
    const pet = TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => {
      const id = petId()
      return id === undefined ? undefined : { petId: id }
    }))
    await settle()

    expect(pet.status()).toBe('idle')
    expect(pet.value()).toBeUndefined()
    http.expectNone(() => true)

    petId.set(1)
    await settle()
    http.expectOne('http://localhost/api/pet/1').flush({ id: 1, name: 'Rex' })
    await settle()
    expect(pet.value()?.name).toBe('Rex')
  })

  it('stays idle while a required parameter is null or undefined', async () => {
    const withNull = TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => ({ petId: null as unknown as number })))
    const withUndefined = TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => ({ petId: undefined as unknown as number })))
    await settle()

    expect(withNull.status()).toBe('idle')
    expect(withUndefined.status()).toBe('idle')
    http.expectNone(() => true)
  })

  it('builds the request from the parameters, the configuration and the options', async () => {
    const context = new HttpContext().set(TOKEN, 'from-options')
    const pet = TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => ({ petId: 7 }), { context, transferCache: false }))
    await settle()

    const req = http.expectOne('http://localhost/api/pet/7')
    expect(req.request.method).toBe('GET')
    expect(req.request.headers.get('api_key')).toBe('secret-key')
    expect(req.request.headers.get('Accept')).toBe('application/json')
    expect(req.request.withCredentials).toBe(true)
    expect(req.request.context.get(TOKEN)).toBe('from-options')
    expect(req.request.transferCache).toBe(false)
    expect(req.request.responseType).toBe('json')

    req.flush({ id: 7, name: 'Rex' })
    await settle()
    expect(pet.status()).toBe('resolved')
    expect(pet.value()).toEqual({ id: 7, name: 'Rex' })
    expect(pet.statusCode()).toBe(200)
  })

  it('uses the transfer cache by default', async () => {
    TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => ({ petId: 7 })))
    await settle()

    const req = http.expectOne('http://localhost/api/pet/7')
    expect(req.request.transferCache).toBe(true)
    req.flush({ id: 7, name: 'Rex' })
    await settle()
  })

  it('sends a new request when a parameter changes', async () => {
    const petId = signal(1)
    const pet = TestBed.runInInjectionContext(() => petService.getPetByIdResource(() => ({ petId: petId() })))
    await settle()

    const first = http.expectOne('http://localhost/api/pet/1')
    petId.set(2)
    await settle()

    expect(first.cancelled).toBe(true)
    http.expectOne('http://localhost/api/pet/2').flush({ id: 2, name: 'Max' })
    await settle()
    expect(pet.value()?.name).toBe('Max')
  })

  it('encodes query parameters with the configured codec', async () => {
    const tags = TestBed.runInInjectionContext(() => petService.listPetsResource(() => ({ tags: ['a b&c', 'd'], xRequestId: 'request-1' })))
    await settle()

    const req = http.expectOne(r => r.url === 'http://localhost/api/pet')
    expect(req.request.urlWithParams).toBe('http://localhost/api/pet?tags=a%20b%26c&tags=d')
    expect(req.request.headers.get('X-Request-Id')).toBe('request-1')
    req.flush([])
    await settle()
    expect(tags.value()).toEqual([])
  })

  it('sends the request without arguments when every parameter is optional', async () => {
    TestBed.runInInjectionContext(() => petService.listPetsResource())
    await settle()

    const req = http.expectOne('http://localhost/api/pet')
    expect(req.request.params.keys()).toEqual([])
    req.flush([])
    await settle()
  })

  it('sends the API key in the query for an operation without parameters', async () => {
    const inventory = TestBed.runInInjectionContext(() => storeService.getInventoryResource())
    await settle()

    const req = http.expectOne(r => r.url === 'http://localhost/api/store/inventory')
    expect(req.request.urlWithParams).toBe('http://localhost/api/store/inventory?api_key=query-key')
    req.flush({ available: 3 })
    await settle()
    expect(inventory.value()).toEqual({ available: 3 })
  })

  it('reads the credentials when the request is built', async () => {
    const byStatus = TestBed.runInInjectionContext(() => petService.findPetsByStatusResource(() => ({ status: ['available', 'sold'] }), { defaultValue: [] }))
    const initial: Pet[] = byStatus.value()
    expect(initial).toEqual([])
    await settle()

    let req = http.expectOne(r => r.url === 'http://localhost/api/pet/findByStatus')
    expect(req.request.urlWithParams).toBe('http://localhost/api/pet/findByStatus?status=available,sold')
    expect(req.request.headers.get('Authorization')).toBe('Bearer first-token')
    req.flush([{ id: 1, name: 'Rex' }])
    await settle()
    expect(byStatus.value().length).toBe(1)

    accessToken.set('second-token')
    await settle()
    req = http.expectOne(r => r.url === 'http://localhost/api/pet/findByStatus')
    expect(req.request.headers.get('Authorization')).toBe('Bearer second-token')
    req.flush([])
    await settle()
  })

  it('uses httpResource.text for a text response', async () => {
    const notes = TestBed.runInInjectionContext(() => petService.getPetNotesResource(() => ({ petId: 3 })))
    await settle()

    const req = http.expectOne('http://localhost/api/pet/3/notes')
    expect(req.request.responseType).toBe('text')
    expect(req.request.headers.get('Accept')).toBe('text/plain')
    req.flush('likes walks')
    await settle()
    expect(notes.value()).toBe('likes walks')
  })

  it('uses httpResource.blob for a binary response', async () => {
    const photo = TestBed.runInInjectionContext(() => petService.getPetPhotoResource(() => ({ petId: 3 })))
    await settle()

    const req = http.expectOne('http://localhost/api/pet/3/photo')
    expect(req.request.responseType).toBe('blob')
    expect(req.request.headers.get('Accept')).toBe('application/octet-stream')
    req.flush(new Blob(['png']))
    await settle()
    expect(photo.value() instanceof Blob).toBe(true)
  })

  it('takes an injector when called outside an injection context', async () => {
    const pet = petService.getPetByIdResource(() => ({ petId: 5 }), { injector: TestBed.inject(Injector) })
    await settle()

    http.expectOne('http://localhost/api/pet/5').flush({ id: 5, name: 'Bo' })
    await settle()
    expect(pet.value()?.name).toBe('Bo')
    pet.destroy()
  })

  it('generates resources for reads only', () => {
    const methods = petService as unknown as Record<string, unknown>
    expect(typeof methods['getPetByIdResource']).toBe('function')
    expect(methods['addPetResource']).toBeUndefined()
    expect(methods['deletePetResource']).toBeUndefined()
    expect(methods['searchPetsResource']).toBeUndefined()
  })
})
