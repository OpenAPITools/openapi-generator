import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { User,  } from '../models';


@Injectable()
export abstract class FakeApi {

  abstract getUsers( request: Request): Array<User> | Promise<Array<User>> | Observable<Array<User>>;

} 