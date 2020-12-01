import { NgModule, ModuleWithProviders, SkipSelf, Optional } from '@angular/core';
import { PetStoreConfiguration } from './configuration';
import { HttpClient } from '@angular/common/http';

import { PetService } from './api/pet.service';
import { StoreService } from './api/store.service';
import { UserService } from './api/user.service';

@NgModule({
  imports:      [],
  declarations: [],
  exports:      [],
  providers: []
})
export class PetStoreApiModule {
    public static forRoot(configurationFactory: () => PetStoreConfiguration): ModuleWithProviders {
        return {
            ngModule: PetStoreApiModule,
            providers: [ { provide: PetStoreConfiguration, useFactory: configurationFactory } ]
        };
    }

    constructor( @Optional() @SkipSelf() parentModule: PetStoreApiModule,
                 @Optional() http: HttpClient) {
        if (parentModule) {
            throw new Error('PetStoreApiModule is already loaded. Import in your base AppModule only.');
        }
        if (!http) {
            throw new Error('You need to import the HttpClientModule in your AppModule! \n' +
            'See also https://github.com/angular/angular/issues/20575');
        }
    }
}
