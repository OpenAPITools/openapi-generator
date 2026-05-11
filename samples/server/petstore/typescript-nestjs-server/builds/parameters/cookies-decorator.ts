import { createParamDecorator, ExecutionContext } from '@nestjs/common';

/**
* A decorator function for retrieving cookies from the request object in an HTTP context.
*
* This decorator only works, if the framework specific cookie middleware is installed and enabled.
* - For Express, you need to use the `cookie-parser` middleware.
* - For Fastify, you need to use the `@fastify/cookie` plugin.
*
* Consult https://docs.nestjs.com/techniques/cookies for further information
*
* Usage:
* ```
* @Get()
* findAll(@Cookies('name') name: string) {}
* ```
*/
export const Cookies = createParamDecorator((data: string, ctx: ExecutionContext) => {
  const request = ctx.switchToHttp().getRequest();
  return data ? request.cookies?.[data] : request.cookies;
});