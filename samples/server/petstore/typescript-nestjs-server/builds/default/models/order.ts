

/**
 * An order for a pets from the pet store
 */
export interface Order { 
  id?: number;
  petId?: number;
  quantity?: number;
  shipDate?: string;
  /**
   * Order Status
   */
  status?: Order.StatusEnum;
  complete?: boolean;
}
export namespace Order {
  export const StatusEnum = {
    Placed: 'placed',
    Approved: 'approved',
    Delivered: 'delivered'
  } as const;
  export type StatusEnum = typeof StatusEnum[keyof typeof StatusEnum];
}


