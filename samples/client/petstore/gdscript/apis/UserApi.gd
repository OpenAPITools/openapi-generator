# API UserApi


class UserApi:
	extends ApiBee


	# Operation createUser  POST /user
	# Create user
	#
	# This can only be done by the logged in user.
	func create_user(
		# user: User
		# Created user object
		user: User,
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user"

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("POST")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation createUsersWithArrayInput  POST /user/createWithArray
	# Creates list of users with given input array
	func create_users_with_array_input(
		# user: Array
		# List of user object
		user: Array,
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/createWithArray"

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("POST")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation createUsersWithListInput  POST /user/createWithList
	# Creates list of users with given input array
	func create_users_with_list_input(
		# user: Array
		# List of user object
		user: Array,
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/createWithList"

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("POST")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation deleteUser  DELETE /user/{username}
	# Delete user
	#
	# This can only be done by the logged in user.
	func delete_user(
		# username: String   Eg: username_example
		# The name that needs to be deleted
		username: String,
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/{username}".replace("{" + "username" + "}", bee_urlize_path_param(username))

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("DELETE")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation getUserByName  GET /user/{username}
	# Get user by user name
	func get_user_by_name(
		# username: String   Eg: username_example
		# The name that needs to be fetched. Use user1 for testing.
		username: String,
		on_success: Callable,  # func(result: User)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/{username}".replace("{" + "username" + "}", bee_urlize_path_param(username))

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("GET")

		# Will be used at some point for denormalization
		# baseType = "User"
		# openApiType = "User"
		# dataType = "User"
		# complexType = "User"
		# isArray = "false"
		var bzz_return_type := "User"

		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation loginUser  GET /user/login
	# Logs user into the system
	func login_user(
		# username: String   Eg: username_example
		# The user name for login
		username: String,
		# password: String   Eg: password_example
		# The password for login in clear text
		password: String,
		on_success: Callable,  # func(result: String)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/login"

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
			"username": username,
			"password": password,
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("GET")

		# Will be used at some point for denormalization
		# baseType = "string"
		# openApiType = "string"
		# dataType = "String"
		# complexType = "string"
		# isArray = "false"
		var bzz_return_type := "string"

		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation logoutUser  GET /user/logout
	# Logs out current logged in user session
	func logout_user(
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/logout"

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("GET")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

	# Operation updateUser  PUT /user/{username}
	# Updated user
	#
	# This can only be done by the logged in user.
	func update_user(
		# username: String   Eg: username_example
		# name that need to be deleted
		username: String,
		# user: User
		# Updated user object
		user: User,
		on_success: Callable,  # func(result)
		on_failure: Callable   # func(error: ApiError)
	):
		# CollectionFormat: NO

		# Note: `bzz_` prefix in variable names is to reduce collisions and therefore renames
		# Warn: Make sure all local variable names here are also listed in our Java CodeGen.

		# Compute the URL path to the API resource
		var bzz_path := "/user/{username}".replace("{" + "username" + "}", bee_urlize_path_param(username))

		# Collect the query parameters
		# Note: we do not support multiple values for a single param (for now), nor arrays
		#var bzz_query := Dictionary()
		var bzz_query := {
		}

		# Convert the HTTP method to something Godot understands
		var bzz_method := bee_convert_http_method("PUT")


		bee_request(
			bzz_method, bzz_path, bzz_query,
			func(result, code, headers):
				#print('SUCCESS!')
				#print(result)
				on_success.call(result)
				,  # ざわ‥
			func(error):
				#printerr("FAILURE!")
				#print(error)
				on_failure.call(error)
				,  # ざわ‥
		)

