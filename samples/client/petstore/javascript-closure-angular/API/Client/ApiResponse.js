goog.provide('API.Client.ApiResponse');

/**
 * Describes the result of uploading an image resource
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

