extends RefCounted
class_name ApiBee  # We name it for now (KISS), but eventually I'd like to remove this.


# Base class for all generated API endpoints.
#
# Every property/method defined here may collide with userland,
# so these are all listed and excluded in our CodeGen Java file.
# We want to keep the amount of renaming to a minimum, though.
# Therefore, we use the bee_ prefix, even if awkward.


const DEFAULT_HOST := "localhost"
const HTTP_DEFAULT_PORT := 80
const HTTPS_DEFAULT_PORT := 443
const POLLING_INTERVAL_MS := 500  # milliseconds


# We'll probably only use this for logging
var bee_name := "ApiBee"


# Godot's HTTP Client we are using.
# If none was set (by you), we'll make one.
var bee_client: HTTPClient:
	set(value):
		bee_client = value
	get:
		if not bee_client:
			bee_client = HTTPClient.new()
		return bee_client


# The host to connect to, with or without the scheme.
# Eg: "gitea.com", "https://gitea.com"
# We configure TLS accordingly to the provided scheme, if any.
var bee_host := DEFAULT_HOST:
	set(value):
		if value.begins_with("https://"):
			bee_enable_ssl()
			value = value.substr(8)  # "https://".length() == 8
		elif value.begins_with("http://"):
			bee_disable_ssl()
			value = value.substr(7)  # "http://".length() == 7
		bee_host = value


# Port through which the connection will be established.
# Note: changing the host may change the port as well.
var bee_port := HTTP_DEFAULT_PORT


# @private
var __bee_ssl_enabled := false
var __bee_verify_host := true  # only if ssl enabled


func bee_enable_ssl():
	self.__bee_ssl_enabled = true
	self.bee_port = HTTPS_DEFAULT_PORT


func bee_disable_ssl():
	self.__bee_ssl_enabled = false
	self.bee_port = HTTP_DEFAULT_PORT


func bee_next_loop_iteration():
	# Use this when it works, on release (beta3 now)
#	return Engine.get_main_loop().idle_frame
	# For now, this works
	return RenderingServer.frame_post_draw


func bee_connect_client_if_needed(
	on_success: Callable,  # func()
	on_failure: Callable,  # func(error: ApiError)
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
		self.bee_host, self.bee_port,
		self.__bee_ssl_enabled, self.__bee_verify_host
	)
	if connecting != OK:
		var error := ApiError.new()
		error.internal_code = connecting
		error.identifier = "apibee.connect_to_host.failure"
		error.message = "%s: failed to connect to `%s' port %d with error %d" % [
			self.bee_name, self.bee_host, self.bee_port, connecting
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
		if POLLING_INTERVAL_MS:
			OS.delay_msec(POLLING_INTERVAL_MS)
		await bee_next_loop_iteration()

	if self.bee_client.get_status() != HTTPClient.STATUS_CONNECTED:
		var error := ApiError.new()
		error.internal_code = connecting
		error.identifier = "apibee.connect_to_host.wrong_status"
		error.message = "%s: failed to connect to `%s' port %d, with client status %d" % [
			self.bee_name, self.bee_host, self.bee_port, self.bee_client.get_status()
		]
		on_failure.call(error)
		return

	on_success.call()


# @protected
func bee_request(
	method: int,  # one of HTTPClient.METHOD_XXXXX
	path: String,
	query: Dictionary,
	on_success: Callable,  # func(response: Variant, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: ApiError)
):

	bee_request_text(
		method, path, query,
		func(responseText, responseCode, responseHeaders):
			var mime: String = responseHeaders['Mime']
			var deserializedResponse: Dictionary
			var denormalizedResponse  # Variant

			if "application/json" == mime:
				var parser := JSON.new()
				var parsing := parser.parse(responseText)
				if OK != parsing:
					var error := ApiError.new()
					error.internal_code = parsing
					error.identifier = "apibee.deserialize.cannot_parse_json"
					error.message = "%s: failed to parse JSON at line %d.\n%s" % [
						self.bee_name, parser.get_error_line(), parser.get_error_message()
					]
					on_failure.call(error)
					return
				deserializedResponse = parser.data
				denormalizedResponse = deserializedResponse  # FIXME
			else:
				var error := ApiError.new()
				error.internal_code = ERR_INVALID_DATA
				error.identifier = "apibee.deserialize.mime_type_unsupported"
				error.message = "%s: mime type `%s' is not supported (yet)" % [
					self.bee_name, mime
				]
				on_failure.call(error)
				return

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
	query: Dictionary,
	on_success: Callable,  # func(responseText: String, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: ApiError)
):
	bee_connect_client_if_needed(
		func():
			bee_do_request_text(method, path, query, on_success, on_failure)
			,
		func(error):
			on_failure.call(error)
			,
	)


# @protected
func bee_do_request_text(
	method: int,  # one of HTTPClient.METHOD_XXXXX
	path: String,
	query: Dictionary,
	on_success: Callable,  # func(responseText: String, responseCode: int, responseHeaders: Dictionary)
	on_failure: Callable,  # func(error: ApiError)
):

	# How can we help users define more / override these?
	# 1. template overrides
	# 2. CLI args
	# 3. YAML Config file
	var headers = [
		"User-Agent: Pirulo/1.0 (Godot)",
		"Accept: */*",
	]

	var path_queried := path
	var query_string := self.bee_client.query_string_from_dict(query)
	if query_string:
		path_queried = "%s?%s" % [path, query_string]

	var requesting := self.bee_client.request(method, path_queried, headers)
	if requesting != OK:
		var error := ApiError.new()
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
		if POLLING_INTERVAL_MS:  # yeah yeah, needs work
			OS.delay_msec(POLLING_INTERVAL_MS)
		await bee_next_loop_iteration()
#		if OS.has_feature("web") or async:

	if not self.bee_client.has_response():
		var error := ApiError.new()
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

	prints("RESPONSE CODE:", response_code)
	prints("RESPONSE HEADERS:", response_headers)

	# TODO: cap the size of this, perhaps?
	var response_bytes := PackedByteArray()

	while self.bee_client.get_status() == HTTPClient.STATUS_BODY:
		self.bee_client.poll()
		var chunk = self.bee_client.read_response_body_chunk()
		if chunk.size() == 0:  # Got nothing, wait for buffers to fill a bit.
			if not false:
				OS.delay_usec(POLLING_INTERVAL_MS)
			else:
				await bee_next_loop_iteration()
		else:  # Yummy data has arrived
			response_bytes = response_bytes + chunk

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

