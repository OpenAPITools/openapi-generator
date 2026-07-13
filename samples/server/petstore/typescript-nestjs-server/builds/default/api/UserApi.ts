import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { User,  } from '../models';


@Injectable()
export abstract class UserApi {

  abstract createUser(user: User,  request: Request): void | Promise<void> | Observable<void>;


  abstract createUsersWithArrayInput(user: Array<User>,  request: Request): void | Promise<void> | Observable<void>;


  abstract createUsersWithListInput(user: Array<User>,  request: Request): void | Promise<void> | Observable<void>;


  abstract deleteUser(username: string,  request: Request): void | Promise<void> | Observable<void>;


  abstract getUserByName(username: string,  request: Request): User | Promise<User> | Observable<User>;


  abstract loginUser(username: string, password: string,  request: Request): string | Promise<string> | Observable<string>;


  abstract logoutUser( request: Request): void | Promise<void> | Observable<void>;


  abstract updateUser(username: string, user: User,  request: Request): void | Promise<void> | Observable<void>;

} 