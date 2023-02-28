import { TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClientModule } from '@angular/common/http';
import {
  ApiModule,
  Configuration,
  ConfigurationParameters,
  PetService,
  StoreService,
  UserService,
} from '@swagger/typescript-angular-petstore';
import { AppComponent } from './app.component';
import {fakePetstoreBackendProviders} from "../test/fakeBackend";

describe('AppComponent', () => {

  const apiConfigurationParams: ConfigurationParameters = {
    // add configuration params here
    apiKeys: { api_key: 'foobar' },
  };

  const apiConfig = new Configuration(apiConfigurationParams);

  const getApiConfig: () => Configuration = () => {
    return apiConfig;
  };


  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientModule,
        ApiModule.forRoot(getApiConfig),
      ],
      providers: [
        PetService,
        StoreService,
        UserService,
        ...fakePetstoreBackendProviders,
      ],
      declarations: [
        AppComponent,
      ],
    }).compileComponents();
  }));

  it('should create the app', waitForAsync(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));

  it('should render title in a h1 tag', waitForAsync(() => {
    const fixture = TestBed.createComponent(AppComponent);
    fixture.detectChanges();
    const compiled = fixture.debugElement.nativeElement;
    expect(compiled.querySelector('h1').textContent).toContain('Welcome to Typescript Angular v11 (provided in root)!');
  }));

  describe(`constructor()`, () => {
    it(`should have a petService provided`, () => {
      const petService = TestBed.inject(PetService);
      expect(petService).toBeTruthy();
    });

    it(`should have a storeService provided`, () => {
      const storeService = TestBed.inject(StoreService);
      expect(storeService).toBeTruthy();
    });

    it(`should have a userService provided`, () => {
      const userService = TestBed.inject(UserService);
      expect(userService).toBeTruthy();
    });
  });

  describe('addPet()', () => {
    it(`should add a new pet`, () => {
      const fixture = TestBed.createComponent(AppComponent);
      const instance = fixture.componentInstance;
      const petService = TestBed.inject(PetService);

      spyOn(petService, 'addPet').and.callThrough();

      fixture.detectChanges();
      instance.addPet();

      expect(petService.addPet).toHaveBeenCalledWith({
        name: `pet`,
        photoUrls: []
      });
    });
  });

});
