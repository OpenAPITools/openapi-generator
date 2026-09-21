import { Body, Controller, DefaultValuePipe, Get, Param, ParseIntPipe, ParseFloatPipe, Query, Req } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Cookies, Headers } from '../decorators';
import { FakeApi } from '../api';
import { User,  } from '../models';

@Controller()
export class FakeApiController {
  constructor(private readonly fakeApi: FakeApi) {}

  @Get('/users')
  getUsers(@Req() request: Request): Array<User> | Promise<Array<User>> | Observable<Array<User>> {
    return this.fakeApi.getUsers(request);
  }

} 