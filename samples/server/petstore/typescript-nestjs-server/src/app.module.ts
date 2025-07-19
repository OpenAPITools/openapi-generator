import { Module } from '@nestjs/common';
import { PetService } from './handlers/PetService';
import { UserService } from './handlers/UserService';
import { StoreService } from './handlers/StoreService';
import { ApiModule } from '../builds/default';

@Module({
  imports: [
    ApiModule.forRoot({
      petApi: PetService,
      userApi: UserService,
      storeApi: StoreService,
    }),
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
