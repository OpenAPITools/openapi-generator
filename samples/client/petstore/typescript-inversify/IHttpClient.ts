import { Observable } from 'rxjs/Observable';
import HttpResponse from './HttpResponse';
import { Headers } from './Headers';

interface IHttpClient {
    get(url:string, headers?: Headers):Observable<HttpResponse>
    post(url:string, body?:{}|FormData, headers?: Headers):Observable<HttpResponse>
    put(url:string, body?:{}, headers?: Headers):Observable<HttpResponse>
    patch(url:string, body?:{}, headers?: Headers):Observable<HttpResponse>
    delete(url:string, headers?: Headers):Observable<HttpResponse>
}

export default IHttpClient
