import { Module } from '@nestjs/common';
import { PetService } from './handlers/PetService';
import { UserService } from './handlers/UserService';
import { StoreService } from './handlers/StoreService';
import { ApiModule } from '../builds/default';
import { TestService } from './TestService';

@Module({
  imports: [
    ApiModule.forRoot({
      apiImplementations: {
        petApi: PetService,
        userApi: UserService,
        storeApi: StoreService,
    },
    providers: [TestService]
}),
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
