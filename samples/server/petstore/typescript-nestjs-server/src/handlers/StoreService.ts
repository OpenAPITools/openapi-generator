import { Observable } from 'rxjs';
import { Injectable } from '@nestjs/common';
import { StoreApi } from '../../builds/default/api';
import { Order } from '../../builds/default/models';

@Injectable()
export class StoreService implements StoreApi {

  deleteOrder(
    orderId: string,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  getInventory(request: Request):
    | { [p: string]: number }
    | Promise<{ [p: string]: number }>
    | Observable<{
        [p: string]: number;
      }> {
    throw new Error('Method not implemented.');
  }

  getOrderById(
    orderId: number,
    request: Request,
  ): Order | Promise<Order> | Observable<Order> {
    throw new Error('Method not implemented.');
  }

  placeOrder(
    order: Order,
    request: Request,
  ): Order | Promise<Order> | Observable<Order> {
    throw new Error('Method not implemented.');
  }
}
