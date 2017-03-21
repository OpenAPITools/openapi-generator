export * from './pet.service';
import { PetService }  from './PetService';
export * from './store.service';
import { StoreService }  from './StoreService';
export * from './user.service';
import { UserService }  from './UserService';
export const APIS = [ PetService, StoreService, UserService, ];
