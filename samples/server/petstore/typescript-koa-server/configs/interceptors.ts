import { InterceptorInterface, Action, Interceptor } from 'routing-controllers'
import { Service } from 'typedi'

@Interceptor()
@Service()
export class AutoAssignJSONInterceptor implements InterceptorInterface {
  intercept(action: Action, content: any): any {
    if (typeof content === 'object')
      return JSON.stringify(Object.assign({ message: 'ok' }, content))
    return JSON.stringify({ message: content })
  }
}
