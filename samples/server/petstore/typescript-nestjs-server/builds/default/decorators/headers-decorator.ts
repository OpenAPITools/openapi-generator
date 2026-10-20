import { createParamDecorator, ExecutionContext } from '@nestjs/common';

/**
* A decorator function for retrieving headers from the request object in an HTTP context.
* Workaround for enabling PipeTransformers on Headers (see https://github.com/nestjs/nest/issues/356)
*
* Usage:
* ```
* @Get()
* findAll(@Headers('name') name: string) {}
* ```
*/
export const Headers = createParamDecorator((headerName: string, ctx: ExecutionContext) => {
  const request = ctx.switchToHttp().getRequest();
  return headerName ? request.headers?.[headerName.toLowerCase()] : request.headers;
});