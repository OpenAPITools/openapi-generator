/**
 * Sinon.JS 2.0.0-pre, 2016/01/16
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @author Contributors: https://github.com/cjohansen/Sinon.JS/blob/master/AUTHORS
 *
 * (The BSD License)
 * 
 * Copyright (c) 2010-2014, Christian Johansen, christian@cjohansen.no
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 *     * Neither the name of Christian Johansen nor the names of his contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.sinon = f()}})(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";

module.exports = function calledInOrder(spies) {
    for (var i = 1, l = spies.length; i < l; i++) {
        if (!spies[i - 1].calledBefore(spies[i]) || !spies[i].called) {
            return false;
        }
    }

    return true;
};

},{}],2:[function(require,module,exports){
"use strict";

var Klass = function () {};

module.exports = function create(proto) {
    Klass.prototype = proto;
    return new Klass();
};

},{}],3:[function(require,module,exports){
"use strict";

var div = typeof document !== "undefined" && document.createElement("div");

function isReallyNaN(val) {
    return val !== val;
}

function isDOMNode(obj) {
    var success = false;

    try {
        obj.appendChild(div);
        success = div.parentNode === obj;
    } catch (e) {
        return false;
    } finally {
        try {
            obj.removeChild(div);
        } catch (e) {
            // Remove failed, not much we can do about that
        }
    }

    return success;
}

function isElement(obj) {
    return div && obj && obj.nodeType === 1 && isDOMNode(obj);
}

var deepEqual = module.exports = function deepEqual(a, b) {
    if (typeof a !== "object" || typeof b !== "object") {
        return isReallyNaN(a) && isReallyNaN(b) || a === b;
    }

    if (isElement(a) || isElement(b)) {
        return a === b;
    }

    if (a === b) {
        return true;
    }

    if ((a === null && b !== null) || (a !== null && b === null)) {
        return false;
    }

    if (a instanceof RegExp && b instanceof RegExp) {
        return (a.source === b.source) && (a.global === b.global) &&
            (a.ignoreCase === b.ignoreCase) && (a.multiline === b.multiline);
    }

    var aString = Object.prototype.toString.call(a);
    if (aString !== Object.prototype.toString.call(b)) {
        return false;
    }

    if (aString === "[object Date]") {
        return a.valueOf() === b.valueOf();
    }

    var prop;
    var aLength = 0;
    var bLength = 0;

    if (aString === "[object Array]" && a.length !== b.length) {
        return false;
    }

    for (prop in a) {
        if (Object.prototype.hasOwnProperty.call(a, prop)) {
            aLength += 1;

            if (!(prop in b)) {
                return false;
            }

            // allow alternative function for recursion
            if (!(arguments[2] || deepEqual)(a[prop], b[prop])) {
                return false;
            }
        }
    }

    for (prop in b) {
        if (Object.prototype.hasOwnProperty.call(b, prop)) {
            bLength += 1;
        }
    }

    return aLength === bLength;
};

deepEqual.use = function (match) {
    return function deepEqual$matcher(a, b) {
        if (match.isMatcher(a)) {
            return a.test(b);
        }

        return deepEqual(a, b, deepEqual$matcher);
    };
};

},{}],4:[function(require,module,exports){
"use strict";

module.exports = {
    injectIntoThis: true,
    injectInto: null,
    properties: ["spy", "stub", "mock", "clock", "server", "requests"],
    useFakeTimers: true,
    useFakeServer: true
};

},{}],5:[function(require,module,exports){
/**
 * Format functions
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @license BSD
 *
 * Copyright (c) 2010-2014 Christian Johansen
 */
"use strict";

var formatio = require("formatio");

var formatter = formatio.configure({
    quoteStrings: false,
    limitChildrenCount: 250
});

module.exports = function format() {
    return formatter.ascii.apply(formatter, arguments);
};

},{"formatio":20}],6:[function(require,module,exports){
"use strict";

module.exports = function functionName(func) {
    var name = func.displayName || func.name;
    var matches;

    // Use function decomposition as a last resort to get function
    // name. Does not rely on function decomposition to work - if it
    // doesn't debugging will be slightly less informative
    // (i.e. toString will say 'spy' rather than 'myFunc').
    if (!name && (matches = func.toString().match(/function ([^\s\(]+)/))) {
        name = matches[1];
    }

    return name;
};


},{}],7:[function(require,module,exports){
"use strict";

module.exports = function toString() {
    var i, prop, thisValue;
    if (this.getCall && this.callCount) {
        i = this.callCount;

        while (i--) {
            thisValue = this.getCall(i).thisValue;

            for (prop in thisValue) {
                if (thisValue[prop] === this) {
                    return prop;
                }
            }
        }
    }

    return this.displayName || "sinon fake";
};

},{}],8:[function(require,module,exports){
"use strict";

var defaultConfig = require("./default-config");

module.exports = function getConfig(custom) {
    var config = {};
    var prop;

    custom = custom || {};

    for (prop in defaultConfig) {
        if (defaultConfig.hasOwnProperty(prop)) {
            config[prop] = custom.hasOwnProperty(prop) ? custom[prop] : defaultConfig[prop];
        }
    }

    return config;
};

},{"./default-config":4}],9:[function(require,module,exports){
"use strict";

module.exports = function getPropertyDescriptor(object, property) {
    var proto = object;
    var descriptor;

    while (proto && !(descriptor = Object.getOwnPropertyDescriptor(proto, property))) {
        proto = Object.getPrototypeOf(proto);
    }
    return descriptor;
};

},{}],10:[function(require,module,exports){
/**
 * Sinon core utilities. For internal use only.
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @license BSD
 *
 * Copyright (c) 2010-2013 Christian Johansen
 */
"use strict";

exports.wrapMethod = require("./wrap-method");

exports.create = require("./create");

exports.deepEqual = require("./deep-equal");

exports.format = require("./format");

exports.functionName = require("./function-name");

exports.functionToString = require("./function-to-string");

exports.objectKeys = require("./object-keys");

exports.getPropertyDescriptor = require("./get-property-descriptor");

exports.getConfig = require("./get-config");

exports.defaultConfig = require("./default-config");

exports.timesInWords = require("./times-in-words");

exports.calledInOrder = require("./called-in-order");

exports.orderByFirstCall = require("./order-by-first-call");

exports.walk = require("./walk");

exports.restore = require("./restore");

},{"./called-in-order":1,"./create":2,"./deep-equal":3,"./default-config":4,"./format":5,"./function-name":6,"./function-to-string":7,"./get-config":8,"./get-property-descriptor":9,"./object-keys":11,"./order-by-first-call":12,"./restore":13,"./times-in-words":14,"./walk":15,"./wrap-method":16}],11:[function(require,module,exports){
"use strict";

var hasOwn = Object.prototype.hasOwnProperty;

module.exports = function objectKeys(obj) {
    if (obj !== Object(obj)) {
        throw new TypeError("sinon.objectKeys called on a non-object");
    }

    var keys = [];
    var key;
    for (key in obj) {
        if (hasOwn.call(obj, key)) {
            keys.push(key);
        }
    }

    return keys;
};

},{}],12:[function(require,module,exports){
"use strict";

module.exports = function orderByFirstCall(spies) {
    return spies.sort(function (a, b) {
        // uuid, won't ever be equal
        var aCall = a.getCall(0);
        var bCall = b.getCall(0);
        var aId = aCall && aCall.callId || -1;
        var bId = bCall && bCall.callId || -1;

        return aId < bId ? -1 : 1;
    });
};

},{}],13:[function(require,module,exports){
"use strict";

function isRestorable(obj) {
    return typeof obj === "function" && typeof obj.restore === "function" && obj.restore.sinon;
}

module.exports = function restore(object) {
    if (object !== null && typeof object === "object") {
        for (var prop in object) {
            if (isRestorable(object[prop])) {
                object[prop].restore();
            }
        }
    } else if (isRestorable(object)) {
        object.restore();
    }
};

},{}],14:[function(require,module,exports){
"use strict";

var array = [null, "once", "twice", "thrice"];

module.exports = function timesInWords(count) {
    return array[count] || (count || 0) + " times";
};

},{}],15:[function(require,module,exports){
"use strict";

function walkInternal(obj, iterator, context, originalObj, seen) {
    var proto, prop;

    if (typeof Object.getOwnPropertyNames !== "function") {
        // We explicitly want to enumerate through all of the prototype's properties
        // in this case, therefore we deliberately leave out an own property check.
        /* eslint-disable guard-for-in */
        for (prop in obj) {
            iterator.call(context, obj[prop], prop, obj);
        }
        /* eslint-enable guard-for-in */

        return;
    }

    Object.getOwnPropertyNames(obj).forEach(function (k) {
        if (!seen[k]) {
            seen[k] = true;
            var target = typeof Object.getOwnPropertyDescriptor(obj, k).get === "function" ?
                originalObj : obj;
            iterator.call(context, target[k], k, target);
        }
    });

    proto = Object.getPrototypeOf(obj);
    if (proto) {
        walkInternal(proto, iterator, context, originalObj, seen);
    }
}

/* Walks the prototype chain of an object and iterates over every own property
 * name encountered. The iterator is called in the same fashion that Array.prototype.forEach
 * works, where it is passed the value, key, and own object as the 1st, 2nd, and 3rd positional
 * argument, respectively. In cases where Object.getOwnPropertyNames is not available, walk will
 * default to using a simple for..in loop.
 *
 * obj - The object to walk the prototype chain for.
 * iterator - The function to be called on each pass of the walk.
 * context - (Optional) When given, the iterator will be called with this object as the receiver.
 */
module.exports = function walk(obj, iterator, context) {
    return walkInternal(obj, iterator, context, obj, {});
};

},{}],16:[function(require,module,exports){
"use strict";

var getPropertyDescriptor = require("./get-property-descriptor");
var objectKeys = require("./object-keys");

var hasOwn = Object.prototype.hasOwnProperty;

function isFunction(obj) {
    return typeof obj === "function" || !!(obj && obj.constructor && obj.call && obj.apply);
}

function mirrorProperties(target, source) {
    for (var prop in source) {
        if (!hasOwn.call(target, prop)) {
            target[prop] = source[prop];
        }
    }
}

// Cheap way to detect if we have ES5 support.
var hasES5Support = "keys" in Object;

module.exports = function wrapMethod(object, property, method) {
    if (!object) {
        throw new TypeError("Should wrap property of object");
    }

    if (typeof method !== "function" && typeof method !== "object") {
        throw new TypeError("Method wrapper should be a function or a property descriptor");
    }

    function checkWrappedMethod(wrappedMethod) {
        var error;

        if (!isFunction(wrappedMethod)) {
            error = new TypeError("Attempted to wrap " + (typeof wrappedMethod) + " property " +
                                property + " as function");
        } else if (wrappedMethod.restore && wrappedMethod.restore.sinon) {
            error = new TypeError("Attempted to wrap " + property + " which is already wrapped");
        } else if (wrappedMethod.calledBefore) {
            var verb = wrappedMethod.returns ? "stubbed" : "spied on";
            error = new TypeError("Attempted to wrap " + property + " which is already " + verb);
        }

        if (error) {
            if (wrappedMethod && wrappedMethod.stackTrace) {
                error.stack += "\n--------------\n" + wrappedMethod.stackTrace;
            }
            throw error;
        }
    }

    var error, wrappedMethod, i;

    // IE 8 does not support hasOwnProperty on the window object and Firefox has a problem
    // when using hasOwn.call on objects from other frames.
    var owned = object.hasOwnProperty ? object.hasOwnProperty(property) : hasOwn.call(object, property);

    if (hasES5Support) {
        var methodDesc = (typeof method === "function") ? {value: method} : method;
        var wrappedMethodDesc = getPropertyDescriptor(object, property);

        if (!wrappedMethodDesc) {
            error = new TypeError("Attempted to wrap " + (typeof wrappedMethod) + " property " +
                                property + " as function");
        } else if (wrappedMethodDesc.restore && wrappedMethodDesc.restore.sinon) {
            error = new TypeError("Attempted to wrap " + property + " which is already wrapped");
        }
        if (error) {
            if (wrappedMethodDesc && wrappedMethodDesc.stackTrace) {
                error.stack += "\n--------------\n" + wrappedMethodDesc.stackTrace;
            }
            throw error;
        }

        var types = objectKeys(methodDesc);
        for (i = 0; i < types.length; i++) {
            wrappedMethod = wrappedMethodDesc[types[i]];
            checkWrappedMethod(wrappedMethod);
        }

        mirrorProperties(methodDesc, wrappedMethodDesc);
        for (i = 0; i < types.length; i++) {
            mirrorProperties(methodDesc[types[i]], wrappedMethodDesc[types[i]]);
        }
        Object.defineProperty(object, property, methodDesc);
    } else {
        wrappedMethod = object[property];
        checkWrappedMethod(wrappedMethod);
        object[property] = method;
        method.displayName = property;
    }

    method.displayName = property;

    // Set up a stack trace which can be used later to find what line of
    // code the original method was created on.
    method.stackTrace = (new Error("Stack Trace for original")).stack;

    method.restore = function () {
        // For prototype properties try to reset by delete first.
        // If this fails (ex: localStorage on mobile safari) then force a reset
        // via direct assignment.
        if (!owned) {
            // In some cases `delete` may throw an error
            try {
                delete object[property];
            } catch (e) {} // eslint-disable-line no-empty
            // For native code functions `delete` fails without throwing an error
            // on Chrome < 43, PhantomJS, etc.
        } else if (hasES5Support) {
            Object.defineProperty(object, property, wrappedMethodDesc);
        }

        if (hasES5Support) {
            var descriptor = getPropertyDescriptor(object, property);
            if (descriptor && descriptor.value === method) {
                object[property] = wrappedMethod;
            }
        }
        else {
        // Use strict equality comparison to check failures then force a reset
        // via direct assignment.
            if (object[property] === method) {
                object[property] = wrappedMethod;
            }
        }
    };

    method.restore.sinon = true;

    if (!hasES5Support) {
        mirrorProperties(method, wrappedMethod);
    }

    return method;
};

},{"./get-property-descriptor":9,"./object-keys":11}],17:[function(require,module,exports){
/**
 * The Sinon "server" mimics a web server that receives requests from
 * sinon.FakeXMLHttpRequest and provides an API to respond to those requests,
 * both synchronously and asynchronously. To respond synchronuously, canned
 * answers have to be provided upfront.
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @license BSD
 *
 * Copyright (c) 2010-2013 Christian Johansen
 */
"use strict";

var push = [].push;
var sinon = require("./core");
var createInstance = require("./core/create");
var format = require("./core/format");

function responseArray(handler) {
    var response = handler;

    if (Object.prototype.toString.call(handler) !== "[object Array]") {
        response = [200, {}, handler];
    }

    if (typeof response[2] !== "string") {
        throw new TypeError("Fake server response body should be string, but was " +
                            typeof response[2]);
    }

    return response;
}

var wloc = typeof window !== "undefined" ? window.location : {};
var rCurrLoc = new RegExp("^" + wloc.protocol + "//" + wloc.host);

function matchOne(response, reqMethod, reqUrl) {
    var rmeth = response.method;
    var matchMethod = !rmeth || rmeth.toLowerCase() === reqMethod.toLowerCase();
    var url = response.url;
    var matchUrl = !url || url === reqUrl || (typeof url.test === "function" && url.test(reqUrl));

    return matchMethod && matchUrl;
}

function match(response, request) {
    var requestUrl = request.url;

    if (!/^https?:\/\//.test(requestUrl) || rCurrLoc.test(requestUrl)) {
        requestUrl = requestUrl.replace(rCurrLoc, "");
    }

    if (matchOne(response, this.getHTTPMethod(request), requestUrl)) {
        if (typeof response.response === "function") {
            var ru = response.url;
            var args = [request].concat(ru && typeof ru.exec === "function" ? ru.exec(requestUrl).slice(1) : []);
            return response.response.apply(response, args);
        }

        return true;
    }

    return false;
}

var fakeServer = {
    create: function (config) {
        var server = createInstance(this);
        server.configure(config);
        if (!sinon.xhr.supportsCORS) {
            this.xhr = sinon.useFakeXDomainRequest();
        } else {
            this.xhr = sinon.useFakeXMLHttpRequest();
        }
        server.requests = [];

        this.xhr.onCreate = function (xhrObj) {
            server.addRequest(xhrObj);
        };

        return server;
    },
    configure: function (config) {
        var whitelist = {
            "autoRespond": true,
            "autoRespondAfter": true,
            "respondImmediately": true,
            "fakeHTTPMethods": true
        };
        var setting;

        config = config || {};
        for (setting in config) {
            if (whitelist.hasOwnProperty(setting) && config.hasOwnProperty(setting)) {
                this[setting] = config[setting];
            }
        }
    },
    addRequest: function addRequest(xhrObj) {
        var server = this;
        push.call(this.requests, xhrObj);

        xhrObj.onSend = function () {
            server.handleRequest(this);

            if (server.respondImmediately) {
                server.respond();
            } else if (server.autoRespond && !server.responding) {
                setTimeout(function () {
                    server.responding = false;
                    server.respond();
                }, server.autoRespondAfter || 10);

                server.responding = true;
            }
        };
    },

    getHTTPMethod: function getHTTPMethod(request) {
        if (this.fakeHTTPMethods && /post/i.test(request.method)) {
            var matches = (request.requestBody || "").match(/_method=([^\b;]+)/);
            return matches ? matches[1] : request.method;
        }

        return request.method;
    },

    handleRequest: function handleRequest(xhr) {
        if (xhr.async) {
            if (!this.queue) {
                this.queue = [];
            }

            push.call(this.queue, xhr);
        } else {
            this.processRequest(xhr);
        }
    },

    log: function log(response, request) {
        var str;

        str = "Request:\n" + format(request) + "\n\n";
        str += "Response:\n" + format(response) + "\n\n";

        sinon.log(str);
    },

    respondWith: function respondWith(method, url, body) {
        if (arguments.length === 1 && typeof method !== "function") {
            this.response = responseArray(method);
            return;
        }

        if (!this.responses) {
            this.responses = [];
        }

        if (arguments.length === 1) {
            body = method;
            url = method = null;
        }

        if (arguments.length === 2) {
            body = url;
            url = method;
            method = null;
        }

        push.call(this.responses, {
            method: method,
            url: url,
            response: typeof body === "function" ? body : responseArray(body)
        });
    },

    respond: function respond() {
        if (arguments.length > 0) {
            this.respondWith.apply(this, arguments);
        }

        var queue = this.queue || [];
        var requests = queue.splice(0, queue.length);

        for (var i = 0; i < requests.length; i++) {
            this.processRequest(requests[i]);
        }
    },

    processRequest: function processRequest(request) {
        try {
            if (request.aborted) {
                return;
            }

            var response = this.response || [404, {}, ""];

            if (this.responses) {
                for (var l = this.responses.length, i = l - 1; i >= 0; i--) {
                    if (match.call(this, this.responses[i], request)) {
                        response = this.responses[i].response;
                        break;
                    }
                }
            }

            if (request.readyState !== 4) {
                this.log(response, request);

                request.respond(response[0], response[1], response[2]);
            }
        } catch (e) {
            sinon.logError("Fake server request processing", e);
        }
    },

    restore: function restore() {
        return this.xhr.restore && this.xhr.restore.apply(this.xhr, arguments);
    }
};

module.exports = fakeServer;

},{"./core":10,"./core/create":2,"./core/format":5}],18:[function(require,module,exports){
/**
 * Add-on for sinon.fakeServer that automatically handles a fake timer along with
 * the FakeXMLHttpRequest. The direct inspiration for this add-on is jQuery
 * 1.3.x, which does not use xhr object's onreadystatehandler at all - instead,
 * it polls the object for completion with setInterval. Dispite the direct
 * motivation, there is nothing jQuery-specific in this file, so it can be used
 * in any environment where the ajax implementation depends on setInterval or
 * setTimeout.
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @license BSD
 *
 * Copyright (c) 2010-2013 Christian Johansen
 */
"use strict";

var fakeServer = require("./fake_server");
var fakeTimers = require("./fake_timers");

function Server() {}
Server.prototype = fakeServer;

var fakeServerWithClock = new Server();

fakeServerWithClock.addRequest = function addRequest(xhr) {
    if (xhr.async) {
        if (typeof setTimeout.clock === "object") {
            this.clock = setTimeout.clock;
        } else {
            this.clock = fakeTimers.useFakeTimers();
            this.resetClock = true;
        }

        if (!this.longestTimeout) {
            var clockSetTimeout = this.clock.setTimeout;
            var clockSetInterval = this.clock.setInterval;
            var server = this;

            this.clock.setTimeout = function (fn, timeout) {
                server.longestTimeout = Math.max(timeout, server.longestTimeout || 0);

                return clockSetTimeout.apply(this, arguments);
            };

            this.clock.setInterval = function (fn, timeout) {
                server.longestTimeout = Math.max(timeout, server.longestTimeout || 0);

                return clockSetInterval.apply(this, arguments);
            };
        }
    }

    return fakeServer.addRequest.call(this, xhr);
};

fakeServerWithClock.respond = function respond() {
    var returnVal = fakeServer.respond.apply(this, arguments);

    if (this.clock) {
        this.clock.tick(this.longestTimeout || 0);
        this.longestTimeout = 0;

        if (this.resetClock) {
            this.clock.restore();
            this.resetClock = false;
        }
    }

    return returnVal;
};

fakeServerWithClock.restore = function restore() {
    if (this.clock) {
        this.clock.restore();
    }

    return fakeServer.restore.apply(this, arguments);
};

module.exports = fakeServerWithClock;

},{"./fake_server":17,"./fake_timers":19}],19:[function(require,module,exports){
/**
 * Fake timer API
 * setTimeout
 * setInterval
 * clearTimeout
 * clearInterval
 * tick
 * reset
 * Date
 *
 * Inspired by jsUnitMockTimeOut from JsUnit
 *
 * @author Christian Johansen (christian@cjohansen.no)
 * @license BSD
 *
 * Copyright (c) 2010-2013 Christian Johansen
 */
"use strict";

var llx = require("lolex");

exports.useFakeTimers = function () {
    var now;
    var methods = Array.prototype.slice.call(arguments);

    if (typeof methods[0] === "string") {
        now = 0;
    } else {
        now = methods.shift();
    }

    var clock = llx.install(now || 0, methods);
    clock.restore = clock.uninstall;
    return clock;
};

exports.clock = {
    create: function (now) {
        return llx.createClock(now);
    }
};

exports.timers = {
    setTimeout: setTimeout,
    clearTimeout: clearTimeout,
    setImmediate: (typeof setImmediate !== "undefined" ? setImmediate : undefined),
    clearImmediate: (typeof clearImmediate !== "undefined" ? clearImmediate : undefined),
    setInterval: setInterval,
    clearInterval: clearInterval,
    Date: Date
};

},{"lolex":21}],20:[function(require,module,exports){
(function (global){
((typeof define === "function" && define.amd && function (m) {
    define("formatio", ["samsam"], m);
}) || (typeof module === "object" && function (m) {
    module.exports = m(require("samsam"));
}) || function (m) { this.formatio = m(this.samsam); }
)(function (samsam) {
    "use strict";

    var formatio = {
        excludeConstructors: ["Object", /^.$/],
        quoteStrings: true,
        limitChildrenCount: 0
    };

    var hasOwn = Object.prototype.hasOwnProperty;

    var specialObjects = [];
    if (typeof global !== "undefined") {
        specialObjects.push({ object: global, value: "[object global]" });
    }
    if (typeof document !== "undefined") {
        specialObjects.push({
            object: document,
            value: "[object HTMLDocument]"
        });
    }
    if (typeof window !== "undefined") {
        specialObjects.push({ object: window, value: "[object Window]" });
    }

    function functionName(func) {
        if (!func) { return ""; }
        if (func.displayName) { return func.displayName; }
        if (func.name) { return func.name; }
        var matches = func.toString().match(/function\s+([^\(]+)/m);
        return (matches && matches[1]) || "";
    }

    function constructorName(f, object) {
        var name = functionName(object && object.constructor);
        var excludes = f.excludeConstructors ||
                formatio.excludeConstructors || [];

        var i, l;
        for (i = 0, l = excludes.length; i < l; ++i) {
            if (typeof excludes[i] === "string" && excludes[i] === name) {
                return "";
            } else if (excludes[i].test && excludes[i].test(name)) {
                return "";
            }
        }

        return name;
    }

    function isCircular(object, objects) {
        if (typeof object !== "object") { return false; }
        var i, l;
        for (i = 0, l = objects.length; i < l; ++i) {
            if (objects[i] === object) { return true; }
        }
        return false;
    }

    function ascii(f, object, processed, indent) {
        if (typeof object === "string") {
            var qs = f.quoteStrings;
            var quote = typeof qs !== "boolean" || qs;
            return processed || quote ? '"' + object + '"' : object;
        }

        if (typeof object === "function" && !(object instanceof RegExp)) {
            return ascii.func(object);
        }

        processed = processed || [];

        if (isCircular(object, processed)) { return "[Circular]"; }

        if (Object.prototype.toString.call(object) === "[object Array]") {
            return ascii.array.call(f, object, processed);
        }

        if (!object) { return String((1/object) === -Infinity ? "-0" : object); }
        if (samsam.isElement(object)) { return ascii.element(object); }

        if (typeof object.toString === "function" &&
                object.toString !== Object.prototype.toString) {
            return object.toString();
        }

        var i, l;
        for (i = 0, l = specialObjects.length; i < l; i++) {
            if (object === specialObjects[i].object) {
                return specialObjects[i].value;
            }
        }

        return ascii.object.call(f, object, processed, indent);
    }

    ascii.func = function (func) {
        return "function " + functionName(func) + "() {}";
    };

    ascii.array = function (array, processed) {
        processed = processed || [];
        processed.push(array);
        var pieces = [];
        var i, l;
        l = (this.limitChildrenCount > 0) ? 
            Math.min(this.limitChildrenCount, array.length) : array.length;

        for (i = 0; i < l; ++i) {
            pieces.push(ascii(this, array[i], processed));
        }

        if(l < array.length)
            pieces.push("[... " + (array.length - l) + " more elements]");

        return "[" + pieces.join(", ") + "]";
    };

    ascii.object = function (object, processed, indent) {
        processed = processed || [];
        processed.push(object);
        indent = indent || 0;
        var pieces = [], properties = samsam.keys(object).sort();
        var length = 3;
        var prop, str, obj, i, k, l;
        l = (this.limitChildrenCount > 0) ? 
            Math.min(this.limitChildrenCount, properties.length) : properties.length;

        for (i = 0; i < l; ++i) {
            prop = properties[i];
            obj = object[prop];

            if (isCircular(obj, processed)) {
                str = "[Circular]";
            } else {
                str = ascii(this, obj, processed, indent + 2);
            }

            str = (/\s/.test(prop) ? '"' + prop + '"' : prop) + ": " + str;
            length += str.length;
            pieces.push(str);
        }

        var cons = constructorName(this, object);
        var prefix = cons ? "[" + cons + "] " : "";
        var is = "";
        for (i = 0, k = indent; i < k; ++i) { is += " "; }

        if(l < properties.length)
            pieces.push("[... " + (properties.length - l) + " more elements]");

        if (length + indent > 80) {
            return prefix + "{\n  " + is + pieces.join(",\n  " + is) + "\n" +
                is + "}";
        }
        return prefix + "{ " + pieces.join(", ") + " }";
    };

    ascii.element = function (element) {
        var tagName = element.tagName.toLowerCase();
        var attrs = element.attributes, attr, pairs = [], attrName, i, l, val;

        for (i = 0, l = attrs.length; i < l; ++i) {
            attr = attrs.item(i);
            attrName = attr.nodeName.toLowerCase().replace("html:", "");
            val = attr.nodeValue;
            if (attrName !== "contenteditable" || val !== "inherit") {
                if (!!val) { pairs.push(attrName + "=\"" + val + "\""); }
            }
        }

        var formatted = "<" + tagName + (pairs.length > 0 ? " " : "");
        var content = element.innerHTML;

        if (content.length > 20) {
            content = content.substr(0, 20) + "[...]";
        }

        var res = formatted + pairs.join(" ") + ">" + content +
                "</" + tagName + ">";

        return res.replace(/ contentEditable="inherit"/, "");
    };

    function Formatio(options) {
        for (var opt in options) {
            this[opt] = options[opt];
        }
    }

    Formatio.prototype = {
        functionName: functionName,

        configure: function (options) {
            return new Formatio(options);
        },

        constructorName: function (object) {
            return constructorName(this, object);
        },

        ascii: function (object, processed, indent) {
            return ascii(this, object, processed, indent);
        }
    };

    return Formatio.prototype;
});

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"samsam":22}],21:[function(require,module,exports){
(function (global){
/*global global, window*/
/**
 * @author Christian Johansen (christian@cjohansen.no) and contributors
 * @license BSD
 *
 * Copyright (c) 2010-2014 Christian Johansen
 */

(function (global) {
    "use strict";

    // Make properties writable in IE, as per
    // http://www.adequatelygood.com/Replacing-setTimeout-Globally.html
    // JSLint being anal
    var glbl = global;

    global.setTimeout = glbl.setTimeout;
    global.clearTimeout = glbl.clearTimeout;
    global.setInterval = glbl.setInterval;
    global.clearInterval = glbl.clearInterval;
    global.Date = glbl.Date;

    // setImmediate is not a standard function
    // avoid adding the prop to the window object if not present
    if (global.setImmediate !== undefined) {
        global.setImmediate = glbl.setImmediate;
        global.clearImmediate = glbl.clearImmediate;
    }

    // node expects setTimeout/setInterval to return a fn object w/ .ref()/.unref()
    // browsers, a number.
    // see https://github.com/cjohansen/Sinon.JS/pull/436

    var NOOP = function () { return undefined; };
    var timeoutResult = setTimeout(NOOP, 0);
    var addTimerReturnsObject = typeof timeoutResult === "object";
    clearTimeout(timeoutResult);

    var NativeDate = Date;
    var uniqueTimerId = 1;

    /**
     * Parse strings like "01:10:00" (meaning 1 hour, 10 minutes, 0 seconds) into
     * number of milliseconds. This is used to support human-readable strings passed
     * to clock.tick()
     */
    function parseTime(str) {
        if (!str) {
            return 0;
        }

        var strings = str.split(":");
        var l = strings.length, i = l;
        var ms = 0, parsed;

        if (l > 3 || !/^(\d\d:){0,2}\d\d?$/.test(str)) {
            throw new Error("tick only understands numbers, 'm:s' and 'h:m:s'. Each part must be two digits");
        }

        while (i--) {
            parsed = parseInt(strings[i], 10);

            if (parsed >= 60) {
                throw new Error("Invalid time " + str);
            }

            ms += parsed * Math.pow(60, (l - i - 1));
        }

        return ms * 1000;
    }

    /**
     * Used to grok the `now` parameter to createClock.
     */
    function getEpoch(epoch) {
        if (!epoch) { return 0; }
        if (typeof epoch.getTime === "function") { return epoch.getTime(); }
        if (typeof epoch === "number") { return epoch; }
        throw new TypeError("now should be milliseconds since UNIX epoch");
    }

    function inRange(from, to, timer) {
        return timer && timer.callAt >= from && timer.callAt <= to;
    }

    function mirrorDateProperties(target, source) {
        var prop;
        for (prop in source) {
            if (source.hasOwnProperty(prop)) {
                target[prop] = source[prop];
            }
        }

        // set special now implementation
        if (source.now) {
            target.now = function now() {
                return target.clock.now;
            };
        } else {
            delete target.now;
        }

        // set special toSource implementation
        if (source.toSource) {
            target.toSource = function toSource() {
                return source.toSource();
            };
        } else {
            delete target.toSource;
        }

        // set special toString implementation
        target.toString = function toString() {
            return source.toString();
        };

        target.prototype = source.prototype;
        target.parse = source.parse;
        target.UTC = source.UTC;
        target.prototype.toUTCString = source.prototype.toUTCString;

        return target;
    }

    function createDate() {
        function ClockDate(year, month, date, hour, minute, second, ms) {
            // Defensive and verbose to avoid potential harm in passing
            // explicit undefined when user does not pass argument
            switch (arguments.length) {
            case 0:
                return new NativeDate(ClockDate.clock.now);
            case 1:
                return new NativeDate(year);
            case 2:
                return new NativeDate(year, month);
            case 3:
                return new NativeDate(year, month, date);
            case 4:
                return new NativeDate(year, month, date, hour);
            case 5:
                return new NativeDate(year, month, date, hour, minute);
            case 6:
                return new NativeDate(year, month, date, hour, minute, second);
            default:
                return new NativeDate(year, month, date, hour, minute, second, ms);
            }
        }

        return mirrorDateProperties(ClockDate, NativeDate);
    }

    function addTimer(clock, timer) {
        if (timer.func === undefined) {
            throw new Error("Callback must be provided to timer calls");
        }

        if (!clock.timers) {
            clock.timers = {};
        }

        timer.id = uniqueTimerId++;
        timer.createdAt = clock.now;
        timer.callAt = clock.now + (timer.delay || (clock.duringTick ? 1 : 0));

        clock.timers[timer.id] = timer;

        if (addTimerReturnsObject) {
            return {
                id: timer.id,
                ref: NOOP,
                unref: NOOP
            };
        }

        return timer.id;
    }


    function compareTimers(a, b) {
        // Sort first by absolute timing
        if (a.callAt < b.callAt) {
            return -1;
        }
        if (a.callAt > b.callAt) {
            return 1;
        }

        // Sort next by immediate, immediate timers take precedence
        if (a.immediate && !b.immediate) {
            return -1;
        }
        if (!a.immediate && b.immediate) {
            return 1;
        }

        // Sort next by creation time, earlier-created timers take precedence
        if (a.createdAt < b.createdAt) {
            return -1;
        }
        if (a.createdAt > b.createdAt) {
            return 1;
        }

        // Sort next by id, lower-id timers take precedence
        if (a.id < b.id) {
            return -1;
        }
        if (a.id > b.id) {
            return 1;
        }

        // As timer ids are unique, no fallback `0` is necessary
    }

    function firstTimerInRange(clock, from, to) {
        var timers = clock.timers,
            timer = null,
            id,
            isInRange;

        for (id in timers) {
            if (timers.hasOwnProperty(id)) {
                isInRange = inRange(from, to, timers[id]);

                if (isInRange && (!timer || compareTimers(timer, timers[id]) === 1)) {
                    timer = timers[id];
                }
            }
        }

        return timer;
    }

    function firstTimer(clock) {
        var timers = clock.timers,
            timer = null,
            id;

        for (id in timers) {
            if (timers.hasOwnProperty(id)) {
                if (!timer || compareTimers(timer, timers[id]) === 1) {
                    timer = timers[id];
                }
            }
        }

        return timer;
    }

    function callTimer(clock, timer) {
        var exception;

        if (typeof timer.interval === "number") {
            clock.timers[timer.id].callAt += timer.interval;
        } else {
            delete clock.timers[timer.id];
        }

        try {
            if (typeof timer.func === "function") {
                timer.func.apply(null, timer.args);
            } else {
                eval(timer.func);
            }
        } catch (e) {
            exception = e;
        }

        if (!clock.timers[timer.id]) {
            if (exception) {
                throw exception;
            }
            return;
        }

        if (exception) {
            throw exception;
        }
    }

    function timerType(timer) {
        if (timer.immediate) {
            return "Immediate";
        }
        if (timer.interval !== undefined) {
            return "Interval";
        }
        return "Timeout";
    }

    function clearTimer(clock, timerId, ttype) {
        if (!timerId) {
            // null appears to be allowed in most browsers, and appears to be
            // relied upon by some libraries, like Bootstrap carousel
            return;
        }

        if (!clock.timers) {
            clock.timers = [];
        }

        // in Node, timerId is an object with .ref()/.unref(), and
        // its .id field is the actual timer id.
        if (typeof timerId === "object") {
            timerId = timerId.id;
        }

        if (clock.timers.hasOwnProperty(timerId)) {
            // check that the ID matches a timer of the correct type
            var timer = clock.timers[timerId];
            if (timerType(timer) === ttype) {
                delete clock.timers[timerId];
            } else {
                throw new Error("Cannot clear timer: timer created with set" + ttype + "() but cleared with clear" + timerType(timer) + "()");
            }
        }
    }

    function uninstall(clock, target) {
        var method,
            i,
            l;

        for (i = 0, l = clock.methods.length; i < l; i++) {
            method = clock.methods[i];

            if (target[method].hadOwnProperty) {
                target[method] = clock["_" + method];
            } else {
                try {
                    delete target[method];
                } catch (ignore) {}
            }
        }

        // Prevent multiple executions which will completely remove these props
        clock.methods = [];
    }

    function hijackMethod(target, method, clock) {
        var prop;

        clock[method].hadOwnProperty = Object.prototype.hasOwnProperty.call(target, method);
        clock["_" + method] = target[method];

        if (method === "Date") {
            var date = mirrorDateProperties(clock[method], target[method]);
            target[method] = date;
        } else {
            target[method] = function () {
                return clock[method].apply(clock, arguments);
            };

            for (prop in clock[method]) {
                if (clock[method].hasOwnProperty(prop)) {
                    target[method][prop] = clock[method][prop];
                }
            }
        }

        target[method].clock = clock;
    }

    var timers = {
        setTimeout: setTimeout,
        clearTimeout: clearTimeout,
        setImmediate: global.setImmediate,
        clearImmediate: global.clearImmediate,
        setInterval: setInterval,
        clearInterval: clearInterval,
        Date: Date
    };

    var keys = Object.keys || function (obj) {
        var ks = [],
            key;

        for (key in obj) {
            if (obj.hasOwnProperty(key)) {
                ks.push(key);
            }
        }

        return ks;
    };

    exports.timers = timers;

    function createClock(now) {
        var clock = {
            now: getEpoch(now),
            timeouts: {},
            Date: createDate()
        };

        clock.Date.clock = clock;

        clock.setTimeout = function setTimeout(func, timeout) {
            return addTimer(clock, {
                func: func,
                args: Array.prototype.slice.call(arguments, 2),
                delay: timeout
            });
        };

        clock.clearTimeout = function clearTimeout(timerId) {
            return clearTimer(clock, timerId, "Timeout");
        };

        clock.setInterval = function setInterval(func, timeout) {
            return addTimer(clock, {
                func: func,
                args: Array.prototype.slice.call(arguments, 2),
                delay: timeout,
                interval: timeout
            });
        };

        clock.clearInterval = function clearInterval(timerId) {
            return clearTimer(clock, timerId, "Interval");
        };

        clock.setImmediate = function setImmediate(func) {
            return addTimer(clock, {
                func: func,
                args: Array.prototype.slice.call(arguments, 1),
                immediate: true
            });
        };

        clock.clearImmediate = function clearImmediate(timerId) {
            return clearTimer(clock, timerId, "Immediate");
        };

        clock.tick = function tick(ms) {
            ms = typeof ms === "number" ? ms : parseTime(ms);
            var tickFrom = clock.now, tickTo = clock.now + ms, previous = clock.now;
            var timer = firstTimerInRange(clock, tickFrom, tickTo);
            var oldNow;

            clock.duringTick = true;

            var firstException;
            while (timer && tickFrom <= tickTo) {
                if (clock.timers[timer.id]) {
                    tickFrom = clock.now = timer.callAt;
                    try {
                        oldNow = clock.now;
                        callTimer(clock, timer);
                        // compensate for any setSystemTime() call during timer callback
                        if (oldNow !== clock.now) {
                            tickFrom += clock.now - oldNow;
                            tickTo += clock.now - oldNow;
                            previous += clock.now - oldNow;
                        }
                    } catch (e) {
                        firstException = firstException || e;
                    }
                }

                timer = firstTimerInRange(clock, previous, tickTo);
                previous = tickFrom;
            }

            clock.duringTick = false;
            clock.now = tickTo;

            if (firstException) {
                throw firstException;
            }

            return clock.now;
        };

        clock.next = function next() {
            var timer = firstTimer(clock);
            if (!timer) {
                return clock.now;
            }

            clock.duringTick = true;
            try {
                clock.now = timer.callAt;
                callTimer(clock, timer);
                return clock.now;
            } finally {
                clock.duringTick = false;
            }
        };

        clock.reset = function reset() {
            clock.timers = {};
        };

        clock.setSystemTime = function setSystemTime(now) {
            // determine time difference
            var newNow = getEpoch(now);
            var difference = newNow - clock.now;
            var id, timer;

            // update 'system clock'
            clock.now = newNow;

            // update timers and intervals to keep them stable
            for (id in clock.timers) {
                if (clock.timers.hasOwnProperty(id)) {
                    timer = clock.timers[id];
                    timer.createdAt += difference;
                    timer.callAt += difference;
                }
            }
        };

        return clock;
    }
    exports.createClock = createClock;

    exports.install = function install(target, now, toFake) {
        var i,
            l;

        if (typeof target === "number") {
            toFake = now;
            now = target;
            target = null;
        }

        if (!target) {
            target = global;
        }

        var clock = createClock(now);

        clock.uninstall = function () {
            uninstall(clock, target);
        };

        clock.methods = toFake || [];

        if (clock.methods.length === 0) {
            clock.methods = keys(timers);
        }

        for (i = 0, l = clock.methods.length; i < l; i++) {
            hijackMethod(target, clock.methods[i], clock);
        }

        return clock;
    };

}(global || this));

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],22:[function(require,module,exports){
((typeof define === "function" && define.amd && function (m) { define("samsam", m); }) ||
 (typeof module === "object" &&
      function (m) { module.exports = m(); }) || // Node
 function (m) { this.samsam = m(); } // Browser globals
)(function () {
    var o = Object.prototype;
    var div = typeof document !== "undefined" && document.createElement("div");

    function isNaN(value) {
        // Unlike global isNaN, this avoids type coercion
        // typeof check avoids IE host object issues, hat tip to
        // lodash
        var val = value; // JsLint thinks value !== value is "weird"
        return typeof value === "number" && value !== val;
    }

    function getClass(value) {
        // Returns the internal [[Class]] by calling Object.prototype.toString
        // with the provided value as this. Return value is a string, naming the
        // internal class, e.g. "Array"
        return o.toString.call(value).split(/[ \]]/)[1];
    }

    /**
     * @name samsam.isArguments
     * @param Object object
     *
     * Returns ``true`` if ``object`` is an ``arguments`` object,
     * ``false`` otherwise.
     */
    function isArguments(object) {
        if (getClass(object) === 'Arguments') { return true; }
        if (typeof object !== "object" || typeof object.length !== "number" ||
                getClass(object) === "Array") {
            return false;
        }
        if (typeof object.callee == "function") { return true; }
        try {
            object[object.length] = 6;
            delete object[object.length];
        } catch (e) {
            return true;
        }
        return false;
    }

    /**
     * @name samsam.isElement
     * @param Object object
     *
     * Returns ``true`` if ``object`` is a DOM element node. Unlike
     * Underscore.js/lodash, this function will return ``false`` if ``object``
     * is an *element-like* object, i.e. a regular object with a ``nodeType``
     * property that holds the value ``1``.
     */
    function isElement(object) {
        if (!object || object.nodeType !== 1 || !div) { return false; }
        try {
            object.appendChild(div);
            object.removeChild(div);
        } catch (e) {
            return false;
        }
        return true;
    }

    /**
     * @name samsam.keys
     * @param Object object
     *
     * Return an array of own property names.
     */
    function keys(object) {
        var ks = [], prop;
        for (prop in object) {
            if (o.hasOwnProperty.call(object, prop)) { ks.push(prop); }
        }
        return ks;
    }

    /**
     * @name samsam.isDate
     * @param Object value
     *
     * Returns true if the object is a ``Date``, or *date-like*. Duck typing
     * of date objects work by checking that the object has a ``getTime``
     * function whose return value equals the return value from the object's
     * ``valueOf``.
     */
    function isDate(value) {
        return typeof value.getTime == "function" &&
            value.getTime() == value.valueOf();
    }

    /**
     * @name samsam.isNegZero
     * @param Object value
     *
     * Returns ``true`` if ``value`` is ``-0``.
     */
    function isNegZero(value) {
        return value === 0 && 1 / value === -Infinity;
    }

    /**
     * @name samsam.equal
     * @param Object obj1
     * @param Object obj2
     *
     * Returns ``true`` if two objects are strictly equal. Compared to
     * ``===`` there are two exceptions:
     *
     *   - NaN is considered equal to NaN
     *   - -0 and +0 are not considered equal
     */
    function identical(obj1, obj2) {
        if (obj1 === obj2 || (isNaN(obj1) && isNaN(obj2))) {
            return obj1 !== 0 || isNegZero(obj1) === isNegZero(obj2);
        }
    }


    /**
     * @name samsam.deepEqual
     * @param Object obj1
     * @param Object obj2
     *
     * Deep equal comparison. Two values are "deep equal" if:
     *
     *   - They are equal, according to samsam.identical
     *   - They are both date objects representing the same time
     *   - They are both arrays containing elements that are all deepEqual
     *   - They are objects with the same set of properties, and each property
     *     in ``obj1`` is deepEqual to the corresponding property in ``obj2``
     *
     * Supports cyclic objects.
     */
    function deepEqualCyclic(obj1, obj2) {

        // used for cyclic comparison
        // contain already visited objects
        var objects1 = [],
            objects2 = [],
        // contain pathes (position in the object structure)
        // of the already visited objects
        // indexes same as in objects arrays
            paths1 = [],
            paths2 = [],
        // contains combinations of already compared objects
        // in the manner: { "$1['ref']$2['ref']": true }
            compared = {};

        /**
         * used to check, if the value of a property is an object
         * (cyclic logic is only needed for objects)
         * only needed for cyclic logic
         */
        function isObject(value) {

            if (typeof value === 'object' && value !== null &&
                    !(value instanceof Boolean) &&
                    !(value instanceof Date)    &&
                    !(value instanceof Number)  &&
                    !(value instanceof RegExp)  &&
                    !(value instanceof String)) {

                return true;
            }

            return false;
        }

        /**
         * returns the index of the given object in the
         * given objects array, -1 if not contained
         * only needed for cyclic logic
         */
        function getIndex(objects, obj) {

            var i;
            for (i = 0; i < objects.length; i++) {
                if (objects[i] === obj) {
                    return i;
                }
            }

            return -1;
        }

        // does the recursion for the deep equal check
        return (function deepEqual(obj1, obj2, path1, path2) {
            var type1 = typeof obj1;
            var type2 = typeof obj2;

            // == null also matches undefined
            if (obj1 === obj2 ||
                    isNaN(obj1) || isNaN(obj2) ||
                    obj1 == null || obj2 == null ||
                    type1 !== "object" || type2 !== "object") {

                return identical(obj1, obj2);
            }

            // Elements are only equal if identical(expected, actual)
            if (isElement(obj1) || isElement(obj2)) { return false; }

            var isDate1 = isDate(obj1), isDate2 = isDate(obj2);
            if (isDate1 || isDate2) {
                if (!isDate1 || !isDate2 || obj1.getTime() !== obj2.getTime()) {
                    return false;
                }
            }

            if (obj1 instanceof RegExp && obj2 instanceof RegExp) {
                if (obj1.toString() !== obj2.toString()) { return false; }
            }

            var class1 = getClass(obj1);
            var class2 = getClass(obj2);
            var keys1 = keys(obj1);
            var keys2 = keys(obj2);

            if (isArguments(obj1) || isArguments(obj2)) {
                if (obj1.length !== obj2.length) { return false; }
            } else {
                if (type1 !== type2 || class1 !== class2 ||
                        keys1.length !== keys2.length) {
                    return false;
                }
            }

            var key, i, l,
                // following vars are used for the cyclic logic
                value1, value2,
                isObject1, isObject2,
                index1, index2,
                newPath1, newPath2;

            for (i = 0, l = keys1.length; i < l; i++) {
                key = keys1[i];
                if (!o.hasOwnProperty.call(obj2, key)) {
                    return false;
                }

                // Start of the cyclic logic

                value1 = obj1[key];
                value2 = obj2[key];

                isObject1 = isObject(value1);
                isObject2 = isObject(value2);

                // determine, if the objects were already visited
                // (it's faster to check for isObject first, than to
                // get -1 from getIndex for non objects)
                index1 = isObject1 ? getIndex(objects1, value1) : -1;
                index2 = isObject2 ? getIndex(objects2, value2) : -1;

                // determine the new pathes of the objects
                // - for non cyclic objects the current path will be extended
                //   by current property name
                // - for cyclic objects the stored path is taken
                newPath1 = index1 !== -1
                    ? paths1[index1]
                    : path1 + '[' + JSON.stringify(key) + ']';
                newPath2 = index2 !== -1
                    ? paths2[index2]
                    : path2 + '[' + JSON.stringify(key) + ']';

                // stop recursion if current objects are already compared
                if (compared[newPath1 + newPath2]) {
                    return true;
                }

                // remember the current objects and their pathes
                if (index1 === -1 && isObject1) {
                    objects1.push(value1);
                    paths1.push(newPath1);
                }
                if (index2 === -1 && isObject2) {
                    objects2.push(value2);
                    paths2.push(newPath2);
                }

                // remember that the current objects are already compared
                if (isObject1 && isObject2) {
                    compared[newPath1 + newPath2] = true;
                }

                // End of cyclic logic

                // neither value1 nor value2 is a cycle
                // continue with next level
                if (!deepEqual(value1, value2, newPath1, newPath2)) {
                    return false;
                }
            }

            return true;

        }(obj1, obj2, '$1', '$2'));
    }

    var match;

    function arrayContains(array, subset) {
        if (subset.length === 0) { return true; }
        var i, l, j, k;
        for (i = 0, l = array.length; i < l; ++i) {
            if (match(array[i], subset[0])) {
                for (j = 0, k = subset.length; j < k; ++j) {
                    if (!match(array[i + j], subset[j])) { return false; }
                }
                return true;
            }
        }
        return false;
    }

    /**
     * @name samsam.match
     * @param Object object
     * @param Object matcher
     *
     * Compare arbitrary value ``object`` with matcher.
     */
    match = function match(object, matcher) {
        if (matcher && typeof matcher.test === "function") {
            return matcher.test(object);
        }

        if (typeof matcher === "function") {
            return matcher(object) === true;
        }

        if (typeof matcher === "string") {
            matcher = matcher.toLowerCase();
            var notNull = typeof object === "string" || !!object;
            return notNull &&
                (String(object)).toLowerCase().indexOf(matcher) >= 0;
        }

        if (typeof matcher === "number") {
            return matcher === object;
        }

        if (typeof matcher === "boolean") {
            return matcher === object;
        }

        if (typeof(matcher) === "undefined") {
            return typeof(object) === "undefined";
        }

        if (matcher === null) {
            return object === null;
        }

        if (getClass(object) === "Array" && getClass(matcher) === "Array") {
            return arrayContains(object, matcher);
        }

        if (matcher && typeof matcher === "object") {
            if (matcher === object) {
                return true;
            }
            var prop;
            for (prop in matcher) {
                var value = object[prop];
                if (typeof value === "undefined" &&
                        typeof object.getAttribute === "function") {
                    value = object.getAttribute(prop);
                }
                if (matcher[prop] === null || typeof matcher[prop] === 'undefined') {
                    if (value !== matcher[prop]) {
                        return false;
                    }
                } else if (typeof  value === "undefined" || !match(value, matcher[prop])) {
                    return false;
                }
            }
            return true;
        }

        throw new Error("Matcher was not a string, a number, a " +
                        "function, a boolean or an object");
    };

    return {
        isArguments: isArguments,
        isElement: isElement,
        isDate: isDate,
        isNegZero: isNegZero,
        identical: identical,
        deepEqual: deepEqualCyclic,
        match: match,
        keys: keys
    };
});

},{}]},{},[18])(18)
});
