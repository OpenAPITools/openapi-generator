import {interfaces} from 'inversify';

import { PetService } from './api/pet.service';
import { StoreService } from './api/store.service';
import { UserService } from './api/user.service';

export class ApiServiceBinder {
    public static with(container: interfaces.Container) {
        container.bind<PetService>('PetService').to(PetService).inSingletonScope();
        container.bind<StoreService>('StoreService').to(StoreService).inSingletonScope();
        container.bind<UserService>('UserService').to(UserService).inSingletonScope();
    }
}
