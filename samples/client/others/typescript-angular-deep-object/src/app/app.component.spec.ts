import { TestBed } from '@angular/core/testing';
import { AppComponent } from './app.component';
import { provideHttpClient } from '@angular/common/http';
import { HttpTestingController, provideHttpClientTesting } from '@angular/common/http/testing';

describe('AppComponent', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [AppComponent],
      providers: [
        // ... other test providers
        provideHttpClient(),
        provideHttpClientTesting(),
      ],
    }).compileComponents();
  });

  it('should create the app', async () => {
    const httpTesting = TestBed.inject(HttpTestingController);

    const fixture = TestBed.createComponent(AppComponent);
    await fixture.whenRenderingDone();
    httpTesting.expectOne('http://localhost/car?filter%5Bmake%5D=bmw&filter%5Bmodel%5D=319');
  });
});
