import { Module } from '@nestjs/common';
import { PetService } from './handlers/default/PetService';
import { UserService } from './handlers/default/UserService';
import { StoreService } from './handlers/default/StoreService';
import { ApiModule } from '../builds/default';
import { ApiModule as ParamsModule } from '../builds/parameters';
import { TestService } from './TestService';
import { DefaultService } from './handlers/parameters/DefaultService';

@Module({
  imports: [
    ApiModule.forRoot({
      apiImplementations: {
        petApi: PetService,
        userApi: UserService,
        storeApi: StoreService,
      },
      providers: [TestService],
    }),
    ParamsModule.forRoot({
      apiImplementations: {
        defaultApi: DefaultService,
      },
    }),
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
