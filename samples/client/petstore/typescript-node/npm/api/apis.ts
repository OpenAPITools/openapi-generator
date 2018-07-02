export * from './petApi';
import { PetApi } from './petApi';
export * from './storeApi';
import { StoreApi } from './storeApi';
export * from './userApi';
import { UserApi } from './userApi';
export const APIS = [PetApi, StoreApi, UserApi];
