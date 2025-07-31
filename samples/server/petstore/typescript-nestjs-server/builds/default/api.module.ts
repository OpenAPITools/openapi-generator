import { DynamicModule, Module, Provider } from '@nestjs/common';
import { ApiImplementations } from './api-implementations'
import { PetApi } from './api';
import { PetApiController } from './controllers';
import { StoreApi } from './api';
import { StoreApiController } from './controllers';
import { UserApi } from './api';
import { UserApiController } from './controllers';

@Module({})
export class ApiModule {
  static forRoot(apiImplementations: ApiImplementations): DynamicModule {
      const providers: Provider[] = [
        {
          provide: PetApi,
          useClass: apiImplementations.petApi
        },
        {
          provide: StoreApi,
          useClass: apiImplementations.storeApi
        },
        {
          provide: UserApi,
          useClass: apiImplementations.userApi
        },
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