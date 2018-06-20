import { TestBed } from '@angular/core/testing';
import { HttpClientModule } from '@angular/common/http';
import { ApiModule, Configuration, PetService } from '@swagger/typescript-angular-petstore';

describe('api tests', () => {
  TestBed.configureTestingModule({
    imports: [
      HttpClientModule,
      ApiModule.forRoot(new Configuration())
    ],
    providers: [
      PetService
    ]
  })
});
