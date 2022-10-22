extends RefCounted
class_name DemoApiBee

# Base class for all generated API endpoints.
#
# Every property/method defined here may collide with userland,
# so these are all listed and excluded in our CodeGen Java file.
# We want to keep the amount of renaming to a minimum, though.
# Therefore, we use the bee_ prefix, even if awkward.


const BEE_CONTENT_TYPE_TEXT := "text/plain"
const BEE_CONTENT_TYPE_HTML := "text/html"
const BEE_CONTENT_TYPE_JSON := "application/json"
const BEE_CONTENT_TYPE_FORM := "application/x-www-form-urlencoded"
const BEE_CONTENT_TYPE_JSONLD := "application/json+ld"  # unsupported (for now)
const BEE_CONTENT_TYPE_XML := "application/xml"  # unsupported (for now)

# From this client's point of view.
# Adding a content type here won't magically make the client support it, but you may reorder.
# These are sorted by decreasing preference. (first → preferred)
const BEE_PRODUCIBLE_CONTENT_TYPES := [
	BEE_CONTENT_TYPE_JSON,
	BEE_CONTENT_TYPE_FORM,
]

# From this client's point of view.
# Adding a content type here won't magically make the client support it, but you may reorder.
# These are sorted by decreasing preference. (first → preferred)
const BEE_CONSUMABLE_CONTENT_TYPES := [
	BEE_CONTENT_TYPE_JSON,
]


# We'll probably only use this for logging.
# Each Api child can define its own, and it should be similar to class_name.
var bee_name := "ApiBee"


# Godot's HTTP Client this Api instance is using.
# If none was set (by you), we'll lazily make one.
var bee_client: HTTPClient:
	set(value):
		bee_client = value
	get:
		if not bee_client:
			bee_client = HTTPClient.new()
		return bee_client


# General configuration that can be shared across Api instances for convenience.
# If no configuration was provided, we'll lazily make one with defaults as best we can.
var bee_config: DemoApiConfig:
	set(value):
		bee_config = value
		# TODO: renew client (needs to invalidate on config's property change as well)
	get:
		if not bee_config:
			bee_config = DemoApiConfig.new()
		return bee_config


func bee_next_loop_iteration():
	# I can't find `idle_frame` in 4-beta3, but we probably want idle_frame here
	return Engine.get_main_loop().process_frame


func bee_connect_client_if_needed(
	on_success: Callable,  # func()
	on_failure: Callable,  # func(error: DemoApiError)
	#finally: Callable,
):
	if (
		self.bee_client.get_status() == HTTPClient.STATUS_CONNECTED
		or
		self.bee_client.get_status() == HTTPClient.STATUS_RESOLVING
		or
		self.bee_client.get_status() == HTTPClient.STATUS_CONNECTING
		or
		self.bee_client.get_status() == HTTPClient.STATUS_REQUESTING
		or
		self.bee_client.get_status() == HTTPClient.STATUS_BODY
	):
		on_success.call()

	var connecting := self.bee_client.connect_to_host(
		self.bee_config.host, self.bee_config.port,
		self.bee_config.ssl_enabled, self.bee_config.verify_host
	)
	if connecting != OK:
		var error := DemoApiError.new()
		error.internal_code = connecting
		error.identifier = "apibee.connect_to_host.failure"
		error.message = "%s: failed to connect to `%s' port %d with error %d" % [
			self.bee_name, self.bee_config.host, self.bee_config.port, connecting
		]
		on_failure.call(error)
		return

	# Wait until resolved and connected.
	while (
		self.bee_client.get_status() == HTTPClient.STATUS_CONNECTING
		or
		self.bee_client.get_status() == HTTPClient.STATUS_RESOLVING
	):
		self.bee_client.poll()
		print("Connecting...")
		if self.bee_config.polling_interval_ms:
			OS.delay_msec(self.bee_config.polling_interval_ms)
		await bee_next_loop_iteration()

	if self.bee_client.get_status() != HTTPClient.STATUS_CONNECTED:
		var error := DemoApiError.new()
		error.internal_code = connecting
		error.identifier = "apibee.connect_to_host.wrong_status"
		error.message = "%s: failed to connect to `%s' port %d, with client status %d" % [
			self.bee_name, self.bee_config.host, self.bee_config.port, self.bee_client.get_status()
		]
		on_failure.call(error)
		return

	on_success.call()


# @protected
func bee_request(
	method: int,  # one of HTTPClient.METHOD_XXXXX
	path: String,
	headers: Dictionary,
	query: Dictionary,
	body,  # Variant that will be serialized
	on_success: Callable,  # func(response: Variant, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: DemoApiError)
):

	bee_request_text(
		method, path, headers, query, body,
		func(responseText, responseCode, responseHeaders):
			var mime: String = responseHeaders['Mime']
			var deserializedResponse  # Variant
			var denormalizedResponse  # Variant

			if BEE_CONTENT_TYPE_TEXT == mime:
				deserializedResponse = responseText
			elif BEE_CONTENT_TYPE_HTML == mime:
				deserializedResponse = responseText
			elif BEE_CONTENT_TYPE_JSON == mime:
				var parser := JSON.new()
				var parsing := parser.parse(responseText)
				if OK != parsing:
					var error := DemoApiError.new()
					error.internal_code = parsing
					error.identifier = "apibee.deserialize.cannot_parse_json"
					error.message = "%s: failed to parse JSON at line %d.\n%s" % [
						self.bee_name, parser.get_error_line(), parser.get_error_message()
					]
					on_failure.call(error)
					return
				deserializedResponse = parser.data
			else:
				var error := DemoApiError.new()
				error.internal_code = ERR_INVALID_DATA
				error.identifier = "apibee.deserialize.mime_type_unsupported"
				error.message = "%s: mime type `%s' is not supported (yet)" % [
					self.bee_name, mime
				]
				on_failure.call(error)
				return

			denormalizedResponse = deserializedResponse  # FIXME

			on_success.call(denormalizedResponse, responseCode, responseHeaders)
			,
		func(error):
			on_failure.call(error)
			,
	)


# @protected
func bee_request_text(
	method: int,  # one of HTTPClient.METHOD_XXXXX
	path: String,
	headers: Dictionary,
	query: Dictionary,
	body,  # Variant that will be serialized
	on_success: Callable,  # func(responseText: String, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: DemoApiError)
):
	bee_connect_client_if_needed(
		func():
			bee_do_request_text(method, path, headers, query, body, on_success, on_failure)
			,
		func(error):
			on_failure.call(error)
			,
	)


# @protected
func bee_do_request_text(
	method: int,  # one of HTTPClient.METHOD_XXXXX
	path: String,
	headers: Dictionary,
	query: Dictionary,
	body,  # Variant that will be serialized
	on_success: Callable,  # func(responseText: String, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: DemoApiError)
):

	headers = headers.duplicate(true)
	headers.merge(self.bee_config.headers_base)
	headers.merge(self.bee_config.headers_override, true)

	var body_normalized = body
	if body is Object:
		if body.has_method('bee_collect_missing_properties'):
			var missing_properties : Array = body.bee_collect_missing_properties()
			if missing_properties:
				var error := DemoApiError.new()
				error.identifier = "apibee.request.body.missing_properties"
				error.message = "%s: `%s' is missing required properties %s." % [
					self.bee_name, body.bee_class_name, missing_properties
				]
				on_failure.call(error)
				return
		if body.has_method('bee_normalize'):
			body_normalized = body.bee_normalize()

	var body_serialized := ""
	var content_type := self.bee_get_content_type(headers)
	if content_type == BEE_CONTENT_TYPE_JSON:
		body_serialized = JSON.stringify(body_normalized)
	elif content_type == BEE_CONTENT_TYPE_FORM:
		body_serialized = self.bee_client.query_string_from_dict(body_normalized)
	else:
		# TODO: Handle other serialization schemes (json+ld, xml…)
		push_warning("Unsupported content-type `%s`." % content_type)

	var path_queried := path
	var query_string := self.bee_client.query_string_from_dict(query)
	if query_string:
		path_queried = "%s?%s" % [path, query_string]

	var headers_for_godot := Array()  # of String
	for key in headers:
		headers_for_godot.append("%s: %s" % [key, headers[key]])

	var requesting := self.bee_client.request(method, path_queried, headers_for_godot, body_serialized)
	if requesting != OK:
		var error := DemoApiError.new()
		error.internal_code = requesting
		error.identifier = "apibee.request.failure"
		error.message = "%s: failed to request to path `%s'." % [
			self.bee_name, path
		]
		on_failure.call(error)
		return

	while self.bee_client.get_status() == HTTPClient.STATUS_REQUESTING:
		# Keep polling for as long as the request is being processed.
		self.bee_client.poll()
		print("Requesting...")
		if self.bee_config.polling_interval_ms:
			OS.delay_msec(self.bee_config.polling_interval_ms)
		await bee_next_loop_iteration()
#		if OS.has_feature("web") or async:

	if not self.bee_client.has_response():
		var error := DemoApiError.new()
		error.identifier = "apibee.request.no_response"
		error.message = "%s: request to `%s' yielded no response whatsoever." % [
			self.bee_name, path
		]
		on_failure.call(error)
		return

	var response_code := self.bee_client.get_response_code()
	var response_headers := self.bee_client.get_response_headers_as_dictionary()
	# FIXME: extract from headers "Content-Type": "application/json; charset=utf-8"
	# This begs for a HttpResponse class ; wait for Godot?
	var encoding := "utf-8"
	var mime := "application/json"
	response_headers['Encoding'] = encoding
	response_headers['Mime'] = mime

	# TODO: cap the size of this, perhaps?
	var response_bytes := PackedByteArray()

	while self.bee_client.get_status() == HTTPClient.STATUS_BODY:
		self.bee_client.poll()
		var chunk = self.bee_client.read_response_body_chunk()
		if chunk.size() == 0:  # Got nothing, wait for buffers to fill a bit.
			if self.bee_config.polling_interval_ms:
				OS.delay_usec(self.bee_config.polling_interval_ms)
			await bee_next_loop_iteration()
		else:  # Yummy data has arrived
			response_bytes = response_bytes + chunk

	print("REQUEST %s %s" % [method, path_queried])
	print("Headers: %s" % [str(headers)])
	prints(body_serialized)

	prints("RESPONSE CODE:", response_code)
	prints("RESPONSE HEADERS:", response_headers)
	print("RESPONSE SIZE: %d bytes " % response_bytes.size())

	var response_text: String
	if encoding == "utf-8":
		response_text = response_bytes.get_string_from_utf8()
	elif encoding == "utf-16":
		response_text = response_bytes.get_string_from_utf16()
	elif encoding == "utf-32":
		response_text = response_bytes.get_string_from_utf32()
	else:
		response_text = response_bytes.get_string_from_ascii()

	if response_code >= 500:
		var error := DemoApiError.new()
		error.internal_code = ERR_PRINTER_ON_FIRE
		error.identifier = "apibee.response.5xx"
		error.message = "%s: request to `%s' made the server hiccup with a %d." % [
			self.bee_name, path, response_code
		]
		error.message += "\n%s" % [
			bee_format_error_response(response_text)
		]
		on_failure.call(error)
		return
	elif response_code >= 400:
		var error := DemoApiError.new()
		error.identifier = "apibee.response.4xx"
		error.message = "%s: request to `%s' was denied with a %d." % [
			self.bee_name, path, response_code
		]
		error.message += "\n%s" % [
			bee_format_error_response(response_text)
		]
		on_failure.call(error)
		return
	elif response_code >= 300:
		var error := DemoApiError.new()
		error.identifier = "apibee.response.3xx"
		error.message = "%s: request to `%s' was redirected with a %d.  We do not support redirects in that client yet." % [
			self.bee_name, path, response_code
		]
		on_failure.call(error)
		return

	# Should we close ?
	#self.bee_client.close()

	on_success.call(response_text, response_code, response_headers)


func bee_convert_http_method(method: String) -> int:
	match method:
		'GET': return HTTPClient.METHOD_GET
		'POST': return HTTPClient.METHOD_POST
		'PUT': return HTTPClient.METHOD_PUT
		'PATCH': return HTTPClient.METHOD_PATCH
		'DELETE': return HTTPClient.METHOD_DELETE
		'CONNECT': return HTTPClient.METHOD_CONNECT
		'HEAD': return HTTPClient.METHOD_HEAD
		'MAX': return HTTPClient.METHOD_MAX
		'OPTIONS': return HTTPClient.METHOD_OPTIONS
		'TRACE': return HTTPClient.METHOD_TRACE
		_:
			printerr("%s: unknown http method `%s`, assuming GET." % [
				self.bee_name, method
			])
			return HTTPClient.METHOD_GET


func bee_urlize_path_param(anything) -> String:
	var serialized := bee_escape_path_param(str(anything))
	return serialized


func bee_escape_path_param(value: String) -> String:
	# TODO: escape for URL
	return value


func bee_get_content_type(headers: Dictionary) -> String:
	if headers.has("Content-Type"):
		return headers["Content-Type"]
	return BEE_PRODUCIBLE_CONTENT_TYPES[0]


func bee_format_error_response(response: String) -> String:
	# TODO: handle other (de)serialization schemes
	var parser := JSON.new()
	var parsing := parser.parse(response)
	if OK != parsing:
		return response
	if not (parser.data is Dictionary):
		return response
	var s := "ERROR"
	if parser.data.has("code"):
		s += " %d" % parser.data['code']
	if parser.data.has("message"):
		s += "\n%s" % parser.data['message']
	else:
		return response
	return s
