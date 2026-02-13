import { Body, Controller, DefaultValuePipe, Get, Param, ParseIntPipe, ParseFloatPipe, Query, Req } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Cookies, Headers } from '../decorators';
import { DefaultApi } from '../api';

@Controller()
export class DefaultApiController {
  constructor(private readonly defaultApi: DefaultApi) {}

  @Get('/test/parameters/:path_default/:path_nullable')
  findPetsByStatus(@Param('path_default') pathDefault: string, @Param('path_nullable') pathNullable: string, @Query('query_default', new DefaultValuePipe('available')) queryDefault: string | undefined, @Query('query_default_enum', new DefaultValuePipe('B')) queryDefaultEnum: 'A' | 'B' | 'C' | undefined, @Query('query_default_int', new DefaultValuePipe(3), ParseIntPipe) queryDefaultInt: number | undefined, @Headers('header_default', new DefaultValuePipe('available')) headerDefault: string | undefined, @Headers('header_default_enum', new DefaultValuePipe('B')) headerDefaultEnum: 'A' | 'B' | 'C' | undefined, @Headers('header_default_int', new DefaultValuePipe(3), ParseIntPipe) headerDefaultInt: number | undefined, @Cookies('cookie_default', new DefaultValuePipe('available')) cookieDefault: string | undefined, @Cookies('cookie_default_enum', new DefaultValuePipe('B')) cookieDefaultEnum: 'A' | 'B' | 'C' | undefined, @Cookies('cookie_default_int', new DefaultValuePipe(3), ParseIntPipe) cookieDefaultInt: number | undefined, @Query('query_nullable') queryNullable: string | null | undefined, @Headers('header_nullable') headerNullable: string | null | undefined, @Cookies('cookie_nullable') cookieNullable: string | null | undefined, @Query('$query-$dollar-sign') $query$dollarSign: string | undefined, @Req() request: Request): void | Promise<void> | Observable<void> {
    return this.defaultApi.findPetsByStatus({ pathDefault, pathNullable, queryDefault, queryDefaultEnum, queryDefaultInt, headerDefault, headerDefaultEnum, headerDefaultInt, cookieDefault, cookieDefaultEnum, cookieDefaultInt, queryNullable, headerNullable, cookieNullable, $query$dollarSign, }, request);
  }

} 