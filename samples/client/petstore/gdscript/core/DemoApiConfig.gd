extends Resource
class_name DemoApiConfig

# Configuration options for Api endpoints.
#
# Its purpose is to help you share configuration customizations across Apis.
# Since it is a Resource, you may use `ResourceSaver.save()` and `load()`
# to save it and load it from file, if you want.
#
# Not sure if this should hold the HTTPClient instance or not.  Not for now.

# These are constant, immutable default values.
# To set different values at runtime, use the public properties below.
const BEE_DEFAULT_HOST := "petstore.swagger.io"
const BEE_DEFAULT_PORT_HTTP := 80
const BEE_DEFAULT_PORT_HTTPS := 443
const BEE_DEFAULT_POLLING_INTERVAL_MS := 500  # milliseconds


# The host to connect to, with or without the protocol scheme.
# Eg: "gitea.com", "https://gitea.com"
# We toggle TLS accordingly to the provided scheme, if any.
var host := BEE_DEFAULT_HOST:
	set(value):
		if value.begins_with("https://"):
			ssl_enabled = true
			value = value.substr(8)  # "https://".length() == 8
		elif value.begins_with("http://"):
			ssl_enabled = false
			value = value.substr(7)  # "http://".length() == 7
		host = value


# Port through which the connection will be established.
# Note: changing the host may change the port as well if the scheme was provided, see above.
var port := BEE_DEFAULT_PORT_HTTP


# Headers used as base for all requests made by Api instances using this config.
# Those are the lowest priority headers, and are merged with custom headers provided in the bee_request() method call
# as well as the headers override below, to compute the final, actually sent headers.
var headers_base := {
	# Stigmergy: everyone does what is left to do (like ants do)
	"User-Agent": "Stigmergiac/1.0 (Godot)",
	# For my mental health's sake, only JSON is supported for now
	"Accept": "application/json",
	"Content-Type": "application/json",
	# FIXME: Remove demo cheat code
	"api_key": "special-key",
}


# High-priority headers, they will always override other headers coming from the base above or the method call.
var headers_override := {}


# Duration of sleep between poll() calls.
var polling_interval_ms := BEE_DEFAULT_POLLING_INTERVAL_MS  # milliseconds


# Should we encrypt packets ?  Yes.  Let's encrypt !
var ssl_enabled := false:
	set(value):
		ssl_enabled = value
		port = BEE_DEFAULT_PORT_HTTPS if ssl_enabled else BEE_DEFAULT_PORT_HTTP


var verify_host := true  # only used if ssl is enabled, disable if trouble

