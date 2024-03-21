//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

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
    final _path = r'/user';

    // ignore: prefer_final_locals
    Object? _postBody = user;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await createUserWithHttpInfo(user,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    final _path = r'/user/createWithArray';

    // ignore: prefer_final_locals
    Object? _postBody = user;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await createUsersWithArrayInputWithHttpInfo(user,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    final _path = r'/user/createWithList';

    // ignore: prefer_final_locals
    Object? _postBody = user;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await createUsersWithListInputWithHttpInfo(user,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    final _path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'DELETE',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await deleteUserWithHttpInfo(username,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    final _path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await getUserByNameWithHttpInfo(username,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'User',) as User;
    
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
    final _path = r'/user/login';

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

      __queryParams.addAll(_queryParams('', 'username', username));
      __queryParams.addAll(_queryParams('', 'password', password));

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await loginUserWithHttpInfo(username, password,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'String',) as String;
    
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
    final _path = r'/user/logout';

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Logs out current logged in user session
  ///
  /// 
  Future<void> logoutUser() async {
    final _response = await logoutUserWithHttpInfo();
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    final _path = r'/user/{username}'
      .replaceAll('{username}', username);

    // ignore: prefer_final_locals
    Object? _postBody = user;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'PUT',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await updateUserWithHttpInfo(username, user,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
  }
}
