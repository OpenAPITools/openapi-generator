import {
  DefaultApi,
  FindPetsByStatusRequestParams,
} from '../../../builds/parameters/api';
import { Observable } from 'rxjs';
import { Injectable } from '@nestjs/common';

@Injectable()
export class DefaultService extends DefaultApi {

  lastRequestParams?: FindPetsByStatusRequestParams;

  findPetsByStatus(
    findPetsByStatusRequestParams: FindPetsByStatusRequestParams,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    this.lastRequestParams = findPetsByStatusRequestParams;
  }
}