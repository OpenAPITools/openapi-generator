goog.provide('API.Client.User');

/**
 * A User who is purchasing from the pet store
 * @record
 */
API.Client.User = function() {}

/**
 * @type {!number}
 * @export
 */
API.Client.User.prototype.id;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.username;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.firstName;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.lastName;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.email;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.password;

/**
 * @type {!string}
 * @export
 */
API.Client.User.prototype.phone;

/**
 * User Status
 * @type {!number}
 * @export
 */
API.Client.User.prototype.userStatus;

