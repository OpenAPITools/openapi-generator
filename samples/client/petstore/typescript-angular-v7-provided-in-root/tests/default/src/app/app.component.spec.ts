import { TestBed, async } from '@angular/core/testing';
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


  beforeEach(async(() => {
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

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));

  it(`should have as title 'Typescript Angular v7 (provided in root)'`, async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app.title).toEqual('Typescript Angular v7 (provided in root)');
  }));

  it('should render title in a h1 tag', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    fixture.detectChanges();
    const compiled = fixture.debugElement.nativeElement;
    expect(compiled.querySelector('h1').textContent).toContain('Welcome to Typescript Angular v7 (provided in root)!');
  }));

  describe(`constructor()`, () => {
    it(`should have a petService provided`, () => {
      const petService = TestBed.get(PetService);
      expect(petService).toBeTruthy();
    });

    it(`should have a storeService provided`, () => {
      const storeService = TestBed.get(StoreService);
      expect(storeService).toBeTruthy();
    });

    it(`should have a userService provided`, () => {
      const userService = TestBed.get(UserService);
      expect(userService).toBeTruthy();
    });
  });

  describe('addPet()', () => {
    it(`should add a new pet`, () => {
      const fixture = TestBed.createComponent(AppComponent);
      const instance = fixture.componentInstance;
      const petService = TestBed.get(PetService);

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
