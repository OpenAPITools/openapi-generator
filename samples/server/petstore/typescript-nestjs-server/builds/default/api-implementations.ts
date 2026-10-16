import { Type } from '@nestjs/common';
import { FakeApi } from './api';
import { PetApi } from './api';
import { StoreApi } from './api';
import { UserApi } from './api';

/**
 * Provide this type to {@link ApiModule} to provide your API implementations
**/
export type ApiImplementations = {
  fakeApi: Type<FakeApi>
  petApi: Type<PetApi>
  storeApi: Type<StoreApi>
  userApi: Type<UserApi>
};
