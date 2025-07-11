import { Body, Controller, Delete, Get, Post, Param, Query, Req } from '@nestjs/common';
import { Observable } from 'rxjs';
import { StoreApi } from '../api';
import { Order,  } from '../models';

@Controller()
export class StoreApiController {
  constructor(private readonly storeApi: StoreApi) {}

  @Delete('/store/order/:orderId')
  deleteOrder(@Param('orderId') orderId: string, @Req() request: Request): void | Promise<void> | Observable<void> {
    return this.storeApi.deleteOrder(orderId, request);
  }

  @Get('/store/inventory')
  getInventory(@Req() request: Request): { [key: string]: number; } | Promise<{ [key: string]: number; }> | Observable<{ [key: string]: number; }> {
    return this.storeApi.getInventory(request);
  }

  @Get('/store/order/:orderId')
  getOrderById(@Param('orderId') orderId: number, @Req() request: Request): Order | Promise<Order> | Observable<Order> {
    return this.storeApi.getOrderById(orderId, request);
  }

  @Post('/store/order')
  placeOrder(@Body() order: Order, @Req() request: Request): Order | Promise<Order> | Observable<Order> {
    return this.storeApi.placeOrder(order, request);
  }

} 