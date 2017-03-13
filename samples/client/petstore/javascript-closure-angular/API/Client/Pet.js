goog.provide('API.Client.Pet');

/**
 * A pet for sale in the pet store
 * @record
 */
API.Client.Pet = function() {}

/**
 * @type {!number}
 * @export
 */
API.Client.Pet.prototype.id;

/**
 * @type {!API.Client.Category}
 * @export
 */
API.Client.Pet.prototype.category;

/**
 * @type {!string}
 * @export
 */
API.Client.Pet.prototype.name;

/**
 * @type {!Array<!string>}
 * @export
 */
API.Client.Pet.prototype.photoUrls;

/**
 * @type {!Array<!API.Client.Tag>}
 * @export
 */
API.Client.Pet.prototype.tags;

/**
 * pet status in the store
 * @type {!string}
 * @export
 */
API.Client.Pet.prototype.status;

/** @enum {string} */
API.Client.Pet.StatusEnum = { 
  available: 'available',
  pending: 'pending',
  sold: 'sold',
}
