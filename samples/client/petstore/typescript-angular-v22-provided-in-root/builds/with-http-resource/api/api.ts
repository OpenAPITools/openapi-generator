export * from './pet.service';
import { PetService } from './pet.service';
export * from './store.service';
import { StoreService } from './store.service';
export const APIS = [PetService, StoreService];
