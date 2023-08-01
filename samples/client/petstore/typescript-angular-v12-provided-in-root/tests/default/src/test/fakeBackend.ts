import {Injectable, Provider} from '@angular/core';
import {Observable, Observer} from 'rxjs';
import {
  HTTP_INTERCEPTORS,
  HttpEvent,
  HttpEventType,
  HttpHandler,
  HttpInterceptor,
  HttpRequest
} from '@angular/common/http';
import { TestRequest } from "@angular/common/http/testing";
import { Pet } from "@swagger/typescript-angular-petstore";

@Injectable()
export class FakePetstoreBackendInterceptor implements HttpInterceptor {
  private fakePetstoreBackend = new FakePetstoreBackend()

  constructor() {

  }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    const parsedUrl = new URL(req.url);
    if (parsedUrl.pathname.indexOf('/v2/pet') === 0) {
      if (req.method === 'GET') {
        const pathParts = parsedUrl.pathname.split('/');
        const petId = parseInt(pathParts[pathParts.length-1], 10);
        return this.respond(req, this.fakePetstoreBackend.getPet(petId));
      } else if (req.method === 'POST') {
        return this.respond(req, this.fakePetstoreBackend.addPet(req.body));
      } else if (req.method === 'PUT') {
        return this.respond(req, this.fakePetstoreBackend.updatePet(req.body));
      } else if (req.method === 'DELETE') {
        const pathParts = parsedUrl.pathname.split('/');
        const petId = parseInt(pathParts[pathParts.length-1], 10);
        this.fakePetstoreBackend.deletePet(petId);
        return this.respond(req, {});
      }
    } else if (parsedUrl.pathname.indexOf('/v2/store/inventory') === 0) {
      if (req.method === 'GET') {
        return this.respond(req, {mega: 42});
      }
    } else if (parsedUrl.pathname.indexOf('/v2/user') === 0) {
      if (req.method === 'POST') {
        return this.respond(req, {mega: 42});
      }
    }
    throw new Error('Http call not implemented in fake backend. ' + req.url);
  }

  private respond(request: HttpRequest<any>, response: any): Observable<HttpEvent<any>> {
    return new Observable((observer: Observer<any>) => {
      const testReq = new TestRequest(request, observer);
      observer.next({ type: HttpEventType.Sent } as HttpEvent<any>);
      testReq.flush(response);
      return () => { };
    });
  }
}

@Injectable()
class FakePetstoreBackend {
  private nextId = 1;
  private pets: Map<string, Pet> = new Map();

  public getPet(id: number): Pet {
    return this.pets.get(String(id));
  }

  public addPet(pet: Pet): Pet {
    const id = this.nextId++;
    this.pets.set(String(id), {
      ...pet,
      id
    });
    return this.getPet(id);
  }

  public updatePet(pet: Pet): Pet {
    this.pets.set(String(pet.id), pet);
    return pet;
  }

  public deletePet(id: number): void {
    this.pets.delete(String(id));
  }
}


export const fakePetstoreBackendProviders: Provider[] = [
  {provide: HTTP_INTERCEPTORS, useClass: FakePetstoreBackendInterceptor, multi: true},
];
