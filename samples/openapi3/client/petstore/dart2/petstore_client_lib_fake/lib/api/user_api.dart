//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class UserApi {
  UserApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Create user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [User] user (required):
  ///   Created user object
  Future<Response> createUserWithHttpInfo(User user) async {
    // Verify required params are set.
    if (user == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: user');
    }

    final path = r'/user';

    Object postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Create user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Parameters:
  ///
  /// * [User] user (required):
  ///   Created user object
  Future<void> createUser(User user) async {
    final response = await createUserWithHttpInfo(user);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Creates list of users with given input array
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<Response> createUsersWithArrayInputWithHttpInfo(List<User> user) async {
    // Verify required params are set.
    if (user == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: user');
    }

    final path = r'/user/createWithArray';

    Object postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Creates list of users with given input array
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<void> createUsersWithArrayInput(List<User> user) async {
    final response = await createUsersWithArrayInputWithHttpInfo(user);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Creates list of users with given input array
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<Response> createUsersWithListInputWithHttpInfo(List<User> user) async {
    // Verify required params are set.
    if (user == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: user');
    }

    final path = r'/user/createWithList';

    Object postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Creates list of users with given input array
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<void> createUsersWithListInput(List<User> user) async {
    final response = await createUsersWithListInputWithHttpInfo(user);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Delete user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be deleted
  Future<Response> deleteUserWithHttpInfo(String username) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }

    final path = r'/user/{username}'
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Delete user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be deleted
  Future<void> deleteUser(String username) async {
    final response = await deleteUserWithHttpInfo(username);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Get user by user name
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be fetched. Use user1 for testing.
  Future<Response> getUserByNameWithHttpInfo(String username) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }

    final path = r'/user/{username}'
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Get user by user name
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be fetched. Use user1 for testing.
  Future<User> getUserByName(String username) async {
    final response = await getUserByNameWithHttpInfo(username);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'User',) as User;
        }
    return Future<User>.value(null);
  }

  /// Logs user into the system
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The user name for login
  ///
  /// * [String] password (required):
  ///   The password for login in clear text
  Future<Response> loginUserWithHttpInfo(String username, String password) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }
    if (password == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: password');
    }

    final path = r'/user/login';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('', 'username', username));
      queryParams.addAll(_convertParametersForCollectionFormat('', 'password', password));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Logs user into the system
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The user name for login
  ///
  /// * [String] password (required):
  ///   The password for login in clear text
  Future<String> loginUser(String username, String password) async {
    final response = await loginUserWithHttpInfo(username, password);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'String',) as String;
        }
    return Future<String>.value(null);
  }

  /// Logs out current logged in user session
  ///
  /// Note: This method returns the HTTP [Response].
  Future<Response> logoutUserWithHttpInfo() async {
    final path = r'/user/logout';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Logs out current logged in user session
  Future<void> logoutUser() async {
    final response = await logoutUserWithHttpInfo();
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Updated user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   name that need to be deleted
  ///
  /// * [User] user (required):
  ///   Updated user object
  Future<Response> updateUserWithHttpInfo(String username, User user) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }
    if (user == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: user');
    }

    final path = r'/user/{username}'
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Updated user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   name that need to be deleted
  ///
  /// * [User] user (required):
  ///   Updated user object
  Future<void> updateUser(String username, User user) async {
    final response = await updateUserWithHttpInfo(username, user);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }
}
