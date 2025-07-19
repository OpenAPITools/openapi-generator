import { Observable } from 'rxjs';
import { Injectable } from '@nestjs/common';
import { UserApi } from '../../builds/default/api';
import { User } from '../../builds/default/models';

@Injectable()
export class UserService implements UserApi {
  createUser(
    user: User,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  createUsersWithArrayInput(
    user: Array<User>,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  createUsersWithListInput(
    user: Array<User>,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  deleteUser(
    username: string,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  getUserByName(
    username: string,
    request: Request,
  ): User | Promise<User> | Observable<User> {
    throw new Error('Method not implemented.');
  }

  loginUser(
    username: string,
    password: string,
    request: Request,
  ): string | Promise<string> | Observable<string> {
    throw new Error('Method not implemented.');
  }

  logoutUser(request: Request): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  updateUser(
    username: string,
    user: User,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }
}
