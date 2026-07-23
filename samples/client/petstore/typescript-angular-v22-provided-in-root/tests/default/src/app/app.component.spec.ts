import { TestBed, waitForAsync } from '@angular/core/testing'
import {
  PetService,
  StoreService,
  UserService
} from '@swagger/typescript-angular-petstore'
import { AppComponent } from './app.component'
import { fakePetstoreBackendInterceptorFn } from '../test/fakeBackend'
import { provideHttpClient, withInterceptors } from '@angular/common/http'

describe('AppComponent', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        PetService,
        StoreService,
        UserService,
        provideHttpClient(withInterceptors([fakePetstoreBackendInterceptorFn]))
      ]
    }).compileComponents()
  })

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent)
    const app = fixture.debugElement.componentInstance
    expect(app).toBeTruthy()
  })

  it('should render title in a h1 tag', () => {
    const fixture = TestBed.createComponent(AppComponent)
    fixture.detectChanges()
    const compiled = fixture.debugElement.nativeElement
    expect(compiled.querySelector('h1').textContent).toContain('Welcome to Typescript Angular v16 (provided in root)!')
  })

  describe('constructor()', () => {
    it('should have a petService provided', () => {
      const petService = TestBed.inject(PetService)
      expect(petService).toBeTruthy()
    })

    it('should have a storeService provided', () => {
      const storeService = TestBed.inject(StoreService)
      expect(storeService).toBeTruthy()
    })

    it('should have a userService provided', () => {
      const userService = TestBed.inject(UserService)
      expect(userService).toBeTruthy()
    })
  })

  describe('addPet()', () => {
    it('should add a new pet', () => {
      const fixture = TestBed.createComponent(AppComponent)
      const instance = fixture.componentInstance
      const petService = TestBed.inject(PetService)

      spyOn(petService, 'addPet').and.callThrough()

      fixture.detectChanges()
      instance.addPet()

      expect(petService.addPet).toHaveBeenCalledWith({
        name: 'pet',
        photoUrls: []
      })
    })
  })
})
