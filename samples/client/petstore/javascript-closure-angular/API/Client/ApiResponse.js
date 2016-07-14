goog.provide('API.Client.ApiResponse');

/**
 * @record
 */
API.Client.ApiResponse = function() {}

/**
 * @type {!number}
 * @export
 */
API.Client.ApiResponse.prototype.code;

/**
 * @type {!string}
 * @export
 */
API.Client.ApiResponse.prototype.type;

/**
 * @type {!string}
 * @export
 */
API.Client.ApiResponse.prototype.message;

