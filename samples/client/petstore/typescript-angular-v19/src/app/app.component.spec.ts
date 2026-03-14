import {TestBed} from '@angular/core/testing';
import {AppComponent} from './app.component';
import {provideHttpClient} from '@angular/common/http';
import {Pet, PetService} from '@swagger/typescript-angular-petstore';
import {of} from 'rxjs';

describe('AppComponent', () => {
  let petServiceMock: jasmine.SpyObj<PetService>;
  const pet: Pet = {name: 'name', photoUrls: []};

  beforeEach(async () => {

    petServiceMock = jasmine.createSpyObj(['findPetsByStatus']);
    petServiceMock.findPetsByStatus.and.returnValue(of([pet]) as any);

    await TestBed.configureTestingModule({
      imports: [AppComponent],
      providers: [
        provideHttpClient(),
        {provide: PetService, useValue: petServiceMock}
      ]
    }).compileComponents();
  });

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });

  it(`should have the 'typescript-angular-v19-unit-tests' title`, () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app.title).toEqual('typescript-angular-v19-unit-tests');
  });

  it('should find pets', () => {
    const fixture = TestBed.createComponent(AppComponent);
    fixture.detectChanges();
    expect(fixture.componentInstance.pets![0]).toEqual(pet)
  });
});