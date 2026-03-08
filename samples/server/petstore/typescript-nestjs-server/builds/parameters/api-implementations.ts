import { Type } from '@nestjs/common';
import { DefaultApi } from './api';

/**
 * Provide this type to {@link ApiModule} to provide your API implementations
**/
export type ApiImplementations = {
  defaultApi: Type<DefaultApi>
};
