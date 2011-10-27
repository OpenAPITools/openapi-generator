var ApiInvoker = new function() {
    this.apiServer = null,
    this.loggingEnabled = false,

            this.trace = function(obj) {
                if (this.loggingEnabled && window.console) console.log(obj);
            },

            this.init = function(apiServer, loggingEnabled) {
                if (apiServer != null && apiServer.length > 0) {
                    if (apiServer.substring(apiServer.length - 1) == ("/")) {
                        apiServer = apiServer.substring(0, apiServer.length - 1);
                    }
                    this.apiServer = apiServer;
                    this.loggingEnabled = (loggingEnabled === null) ? false : loggingEnabled;
                }
            },

            this.invokeAPI = function(authToken, resourceURL, method, queryParams, postObject, completionEvent, requestId, returnType, callback) {
                if (this.apiServer == null) {
                    throw new Error("Please call ApiInvoker.init() to initialize the library");
                }

                this.trace("authToken = " + authToken);
                this.trace("resourceURL = " + resourceURL);
                this.trace("method = " + method);
//        this.trace("returnType = " + returnType);
                this.trace("completionEvent = " + completionEvent);
                this.trace("requestId = " + requestId);
                this.trace("queryParams:");
                this.trace(queryParams);
                this.trace("postObject:");
                this.trace(postObject);

                // create queryParam
                var counter = 0;
                var symbol = 0;
                for (var paramName in queryParams) {
                    var paramValue = queryParams[paramName];
                    symbol = "&";
                    if (counter == 0) {
                        symbol = "?";
                    }
                    resourceURL = resourceURL + symbol + paramName + "=" + paramValue.toString();
                    counter++;
                }

                var callURL = this.apiServer + resourceURL;
                var responseDataType = (returnType === null || returnType == String) ? "html" : "json";

                this.trace("callURL = " + callURL);
                this.trace("responseDataType = " + responseDataType);

                if (method == "GET") {
                    $.get(callURL, postObject,
                            function(response) {
                                ApiInvoker.fire(completionEvent, returnType, requestId, response, callback);
                            }, responseDataType).complete(this.showCompleteStatus).error(this.showErrorStatus);
//                    $.getJSON(callURL,
//                            function(response) {
//                                ApiInvoker.fire(completionEvent, returnType, requestId, response);
//                            }).complete(this.showCompleteStatus).error(this.showErrorStatus);
                } else if (method == "POST") {
                    this.trace("sending post");
                    this.trace(JSON.stringify(postObject));
                    $.ajax({
                        url: callURL,
                        data: JSON.stringify(postObject),
                        type: "POST",
                        dataType: "json",
                        contentType: "application/json",
                        success: function(response) {
                            ApiInvoker.fire(completionEvent, returnType, requestId, response, callback);
                        }
                    }).complete(this.showCompleteStatus).error(this.showErrorStatus);
                } else if (method == "PUT") {
                    $.ajax({
                        url: callURL,
                        data: JSON.stringify(postObject),
                        type: "PUT",
                        dataType: "json",
                        contentType: "application/json",
                        success: function(response) {
                            ApiInvoker.fire(completionEvent, returnType, requestId, response, callback);
                        }
                    }).complete(this.showCompleteStatus).error(this.showErrorStatus);
                } else if (method == "DELETE") {
                    $.ajax({
                        url: callURL,
                        data: JSON.stringify(postObject),
                        type: "DELETE",
                        dataType: "json",
                        contentType: "application/json",
                        success: function(response) {
                            ApiInvoker.fire(completionEvent, returnType, requestId, response, callback);
                        }
                    }).complete(this.showCompleteStatus).error(this.showErrorStatus);
                }


            },

            this.guid = function() {
                return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
                        function(c) {
                            var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
                            return v.toString(16);
                        }).toUpperCase();
            },

            this._listeners = {},

            this.addListener = function(type, listener) {
                if (!this._listeners[type]) {
                    this._listeners[type] = [];
                }

                this._listeners[type].push(listener);
            },

            this.fire = function(completionEvent, returnType, requestId, response, callback) {
                var event = new Object();
                event.type = completionEvent;
                event.requestId = requestId;
                if (returnType === null || returnType == String) {
                    event.data = response;
                } else {
                    event.data = eval("new " + returnType);
                    event.data.parse(response);
                }

                if (typeof event == "string") {
                    event = { type: event };
                }

                if (!event.target) {
                    event.target = this;
                }

                if (!event.type) {
                    throw new Error("Event object must have 'type' property");
                }

                this.trace(event.data);
                if(callback) {
                    this.trace("invoking callback");
                    callback(event);
                } else {
                    this.trace("firing event " + event.type);

                    if (this._listeners[event.type]) {
                        var listeners = this._listeners[event.type];
                        for (var i = 0; i < listeners.length; i++) {
                            listeners[i].call(this, event);
                        }
                    }
                }

            },

            this.removeListener = function(type, listener) {
                if (this._listeners[type]) {
                    var listeners = this._listeners[type];
                    for (var i = 0; i < listeners.length; i++) {
                        if (listeners[i] === listener) {
                            listeners.splice(i, 1);
                            break;
                        }
                    }
                }
            },


            this.showErrorStatus = function(data) {
                ApiInvoker.trace(data);
                if (data.status != 200)
                    ApiInvoker.trace(data.status);
                ApiInvoker.showStatus(data);
            },

            this.showCompleteStatus = function(data) {
                ApiInvoker.trace("complete " + data.status);
//        ApiInvoker.showStatus(data);
            },

            this.showStatus = function(data) {
                ApiInvoker.trace(data);
                ApiInvoker.trace(data.getAllResponseHeaders());
            },

            this.toPathValue = function(value) {
                if(typeof value == Array){
                    return this.arrayToPathValue(value);
                } else
                    return value == null ? "" : value.toString();
            },

            this.arrayToPathValue = function(objects) {
                return objects.join(",");
            }


};
