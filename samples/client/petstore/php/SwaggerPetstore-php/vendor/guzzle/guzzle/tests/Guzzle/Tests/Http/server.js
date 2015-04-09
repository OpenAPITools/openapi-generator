/**
 * Guzzle node.js test server to return queued responses to HTTP requests and
 * expose a RESTful API for enqueueing responses and retrieving the requests
 * that have been received.
 *
 * - Delete all requests that have been received:
 *      DELETE /guzzle-server/requests
 *      Host: 127.0.0.1:8124
 *
 *  - Enqueue responses
 *      PUT /guzzle-server/responses
 *      Host: 127.0.0.1:8124
 *
 *      [{ "statusCode": 200, "reasonPhrase": "OK", "headers": {}, "body": "" }]
 *
 *  - Get the received requests
 *      GET /guzzle-server/requests
 *      Host: 127.0.0.1:8124
 *
 *  - Shutdown the server
 *      DELETE /guzzle-server
 *      Host: 127.0.0.1:8124
 *
 * @package Guzzle PHP <http://www.guzzlephp.org>
 * @license See the LICENSE file that was distributed with this source code.
 */

var http = require("http");

/**
 * Guzzle node.js server
 * @class
 */
var GuzzleServer = function(port, log) {

    this.port = port;
    this.log = log;
    this.responses = [];
    this.requests = [];
    var that = this;

    var controlRequest = function(request, req, res) {
        if (req.url == '/guzzle-server/perf') {
            res.writeHead(200, "OK", {"Content-Length": 16});
            res.end("Body of response");
        } else if (req.method == "DELETE") {
            if (req.url == "/guzzle-server/requests") {
                // Clear the received requests
                that.requests = [];
                res.writeHead(200, "OK", { "Content-Length": 0 });
                res.end();
                if (this.log) {
                    console.log("Flushing requests");
                }
            } else if (req.url == "/guzzle-server") {
                // Shutdown the server
                res.writeHead(200, "OK", { "Content-Length": 0, "Connection": "close" });
                res.end();
                if (this.log) {
                    console.log("Shutting down");
                }
                that.server.close();
            }
        } else if (req.method == "GET") {
            if (req.url === "/guzzle-server/requests") {
                // Get received requests
                var data = that.requests.join("\n----[request]\n");
                res.writeHead(200, "OK", { "Content-Length": data.length });
                res.end(data);
                if (that.log) {
                    console.log("Sending receiving requests");
                }
            }
        } else if (req.method == "PUT") {
            if (req.url == "/guzzle-server/responses") {
                if (that.log) {
                    console.log("Adding responses...");
                }
                // Received response to queue
                var data = request.split("\r\n\r\n")[1];
                if (!data) {
                    if (that.log) {
                        console.log("No response data was provided");
                    }
                    res.writeHead(400, "NO RESPONSES IN REQUEST", { "Content-Length": 0 });
                } else {
                    that.responses = eval("(" + data + ")");
                    if (that.log) {
                        console.log(that.responses);
                    }
                    res.writeHead(200, "OK", { "Content-Length": 0 });
                }
                res.end();
            }
        }
    };

    var receivedRequest = function(request, req, res) {
        if (req.url.indexOf("/guzzle-server") === 0) {
            controlRequest(request, req, res);
        } else if (req.url.indexOf("/guzzle-server") == -1 && !that.responses.length) {
            res.writeHead(500);
            res.end("No responses in queue");
        } else {
            var response = that.responses.shift();
            res.writeHead(response.statusCode, response.reasonPhrase, response.headers);
            res.end(response.body);
            that.requests.push(request);
        }
    };

    this.start = function() {

        that.server = http.createServer(function(req, res) {

            var request = req.method + " " + req.url + " HTTP/" + req.httpVersion + "\r\n";
            for (var i in req.headers) {
                request += i + ": " + req.headers[i] + "\r\n";
            }
            request += "\r\n";

            // Receive each chunk of the request body
            req.addListener("data", function(chunk) {
                request += chunk;
            });

            // Called when the request completes
            req.addListener("end", function() {
                receivedRequest(request, req, res);
            });
        });
        that.server.listen(port, "127.0.0.1");

        if (this.log) {
            console.log("Server running at http://127.0.0.1:8124/");
        }
    };
};

// Get the port from the arguments
port = process.argv.length >= 3 ? process.argv[2] : 8124;
log = process.argv.length >= 4 ? process.argv[3] : false;

// Start the server
server = new GuzzleServer(port, log);
server.start();
