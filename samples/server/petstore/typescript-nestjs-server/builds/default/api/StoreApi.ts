import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Order,  } from '../models';


@Injectable()
export abstract class StoreApi {

  abstract deleteOrder(orderId: string,  request: Request): void | Promise<void> | Observable<void>;


  abstract getInventory( request: Request): Record<string, number> | Promise<Record<string, number>> | Observable<Record<string, number>>;


  abstract getOrderById(orderId: number,  request: Request): Order | Promise<Order> | Observable<Order>;


  abstract placeOrder(order: Order,  request: Request): Order | Promise<Order> | Observable<Order>;

} 