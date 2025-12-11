import { DynamicModule, Module, Provider } from '@nestjs/common';
import { ApiImplementations } from './api-implementations'
import { PetApi } from './api';
import { PetApiController } from './controllers';
import { StoreApi } from './api';
import { StoreApiController } from './controllers';
import { UserApi } from './api';
import { UserApiController } from './controllers';

export type ApiModuleConfiguration = {
  /**
  * your Api implementations
  */
  apiImplementations: ApiImplementations,
  /**
  * additional Providers that may be used by your implementations
  */
  providers?: Provider[],
}

@Module({})
export class ApiModule {
  static forRoot(configuration: ApiModuleConfiguration): DynamicModule {
      const providers: Provider[] = [
        {
          provide: PetApi,
          useClass: configuration.apiImplementations.petApi
        },
        {
          provide: StoreApi,
          useClass: configuration.apiImplementations.storeApi
        },
        {
          provide: UserApi,
          useClass: configuration.apiImplementations.userApi
        },
        ...(configuration.providers || []),
      ];

      return {
        module: ApiModule,
        controllers: [
          PetApiController,
          StoreApiController,
          UserApiController,
        ],
        providers: [...providers],
        exports: [...providers]
      }
    }
}