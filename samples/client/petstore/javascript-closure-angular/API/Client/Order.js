goog.provide('API.Client.Order');

/**
 * An order for a pets from the pet store
 * @record
 */
API.Client.Order = function() {}

/**
 * @type {!number}
 * @export
 */
API.Client.Order.prototype.id;

/**
 * @type {!number}
 * @export
 */
API.Client.Order.prototype.petId;

/**
 * @type {!number}
 * @export
 */
API.Client.Order.prototype.quantity;

/**
 * @type {!Date}
 * @export
 */
API.Client.Order.prototype.shipDate;

/**
 * Order Status
 * @type {!string}
 * @export
 */
API.Client.Order.prototype.status;

/**
 * @type {!boolean}
 * @export
 */
API.Client.Order.prototype.complete;

/** @enum {string} */
API.Client.Order.StatusEnum = { 
  placed: 'placed',
  approved: 'approved',
  delivered: 'delivered',
}
