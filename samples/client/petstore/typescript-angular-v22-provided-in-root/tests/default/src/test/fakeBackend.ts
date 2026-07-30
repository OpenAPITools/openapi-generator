import { Injectable} from '@angular/core'
import { Observable, type Observer } from 'rxjs'
import {
  type HttpEvent,
  HttpEventType,
  type HttpHandlerFn,
  type HttpInterceptorFn,
  type HttpRequest
} from '@angular/common/http'
import { TestRequest } from '@angular/common/http/testing'
import { type Pet } from '@swagger/typescript-angular-petstore'

export const fakePetstoreBackendInterceptorFn: HttpInterceptorFn = (
  req: HttpRequest<unknown>,
  _next: HttpHandlerFn
): Observable<HttpEvent<unknown>> => {
  const parsedUrl = new URL(req.url)

  if (parsedUrl.pathname.indexOf('/v2/pet') === 0) {
    if (req.method === 'GET') {
      const pathParts = parsedUrl.pathname.split('/')
      const petId = parseInt(pathParts[pathParts.length - 1], 10)
      return respond(req, fakePetstoreBackend.getPet(petId))
    } else if (req.method === 'POST') {
      return respond(req, fakePetstoreBackend.addPet(req.body as Pet))
    } else if (req.method === 'PUT') {
      return respond(req, fakePetstoreBackend.updatePet(req.body as Pet))
    } else if (req.method === 'DELETE') {
      const pathParts = parsedUrl.pathname.split('/')
      const petId = parseInt(pathParts[pathParts.length - 1], 10)
      fakePetstoreBackend.deletePet(petId)
      return respond(req, {})
    }
  } else if (parsedUrl.pathname.indexOf('/v2/store/inventory') === 0) {
    if (req.method === 'GET') {
      return respond(req, { mega: 42 })
    }
  } else if (parsedUrl.pathname.indexOf('/v2/user') === 0) {
    if (req.method === 'POST') {
      return respond(req, { mega: 42 })
    }
  }

  throw new Error('Http call not implemented in fake backend. ' + req.url)
}

@Injectable()
class FakePetstoreBackend {
  private nextId = 1
  private readonly pets = new Map<string, Pet>()

  public getPet (id: number): Pet {
    return this.pets.get(String(id))!
  }

  public addPet (pet: Pet): Pet {
    const id = this.nextId++
    this.pets.set(String(id), {
      ...pet,
      id
    })
    return this.getPet(id)
  }

  public updatePet (pet: Pet): Pet {
    this.pets.set(String(pet.id), pet)
    return pet
  }

  public deletePet (id: number): void {
    this.pets.delete(String(id))
  }
}

const fakePetstoreBackend = new FakePetstoreBackend()

function respond (request: HttpRequest<any>, response: any): Observable<HttpEvent<any>> {
  return new Observable((observer: Observer<any>) => {
    const testReq = new TestRequest(request, observer)
    observer.next({ type: HttpEventType.Sent } as HttpEvent<any>)
    testReq.flush(response)
    return () => { }
  })
}
