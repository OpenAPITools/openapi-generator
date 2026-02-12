import { Body, Controller, Get, Param, Query, Headers, Req } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Cookies } from '../cookies-decorator';
import { DefaultApi } from '../api';

@Controller()
export class DefaultApiController {
  constructor(private readonly defaultApi: DefaultApi) {}

  @Get('/test/parameters/:path_default/:path_nullable')
  findPetsByStatus(@Param('path_default') pathDefault: string, @Param('path_nullable') pathNullable: string, @Query('query_default') queryDefault: string | undefined = 'available', @Query('query_default_enum') queryDefaultEnum: 'A' | 'B' | 'C' | undefined = 'B', @Query('query_default_int') queryDefaultInt: number | undefined = 3, @Headers('header_default') headerDefault: string | undefined = 'available', @Headers('header_default_enum') headerDefaultEnum: 'A' | 'B' | 'C' | undefined = 'B', @Headers('header_default_int') headerDefaultInt: number | undefined = 3, @Cookies('cookie_default') cookieDefault: string | undefined = 'available', @Cookies('cookie_default_enum') cookieDefaultEnum: 'A' | 'B' | 'C' | undefined = 'B', @Cookies('cookie_default_int') cookieDefaultInt: number | undefined = 3, @Query('query_nullable') queryNullable: string | null | undefined, @Headers('header_nullable') headerNullable: string | null | undefined, @Cookies('cookie_nullable') cookieNullable: string | null | undefined, @Query('$query-$dollar-sign') $query$dollarSign: string | undefined, @Req() request: Request): void | Promise<void> | Observable<void> {
    return this.defaultApi.findPetsByStatus({ pathDefault, pathNullable, queryDefault, queryDefaultEnum, queryDefaultInt, headerDefault, headerDefaultEnum, headerDefaultInt, cookieDefault, cookieDefaultEnum, cookieDefaultInt, queryNullable, headerNullable, cookieNullable, $query$dollarSign, }, request);
  }

} 