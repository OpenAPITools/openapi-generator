//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class UserApi {
  UserApi([ApiClient? apiClient]) : apiClient = apiClient ?? defaultApiClient;

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
  Future<Response> createUserWithHttpInfo(User user,) async {
    // ignore: prefer_const_declarations
    final path = r'/user';

    // ignore: prefer_final_locals
    Object? postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
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
  Future<void> createUser(User user,) async {
    final response = await createUserWithHttpInfo(user,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Creates list of users with given input array
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<Response> createUsersWithArrayInputWithHttpInfo(List<User> user,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/createWithArray';

    // ignore: prefer_final_locals
    Object? postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
    );
  }

  /// Creates list of users with given input array
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<void> createUsersWithArrayInput(List<User> user,) async {
    final response = await createUsersWithArrayInputWithHttpInfo(user,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Creates list of users with given input array
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<Response> createUsersWithListInputWithHttpInfo(List<User> user,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/createWithList';

    // ignore: prefer_final_locals
    Object? postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
    );
  }

  /// Creates list of users with given input array
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [List<User>] user (required):
  ///   List of user object
  Future<void> createUsersWithListInput(List<User> user,) async {
    final response = await createUsersWithListInputWithHttpInfo(user,);
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
  Future<Response> deleteUserWithHttpInfo(String username,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
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
  Future<void> deleteUser(String username,) async {
    final response = await deleteUserWithHttpInfo(username,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Get user by user name
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be fetched. Use user1 for testing.
  Future<Response> getUserByNameWithHttpInfo(String username,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
    );
  }

  /// Get user by user name
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be fetched. Use user1 for testing.
  Future<User?> getUserByName(String username,) async {
    final response = await getUserByNameWithHttpInfo(username,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body.isNotEmpty && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'User',) as User;
    
    }
    return null;
  }

  /// Logs user into the system
  ///
  /// 
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
  Future<Response> loginUserWithHttpInfo(String username, String password,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/login';

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_queryParams('', 'username', username));
      queryParams.addAll(_queryParams('', 'password', password));

    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
    );
  }

  /// Logs user into the system
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The user name for login
  ///
  /// * [String] password (required):
  ///   The password for login in clear text
  Future<String?> loginUser(String username, String password,) async {
    final response = await loginUserWithHttpInfo(username, password,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body.isNotEmpty && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'String',) as String;
    
    }
    return null;
  }

  /// Logs out current logged in user session
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  Future<Response> logoutUserWithHttpInfo() async {
    // ignore: prefer_const_declarations
    final path = r'/user/logout';

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
    );
  }

  /// Logs out current logged in user session
  ///
  /// 
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
  Future<Response> updateUserWithHttpInfo(String username, User user,) async {
    // ignore: prefer_const_declarations
    final path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
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
  Future<void> updateUser(String username, User user,) async {
    final response = await updateUserWithHttpInfo(username, user,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }
}
