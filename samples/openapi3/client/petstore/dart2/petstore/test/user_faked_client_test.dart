import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'fake_client.dart';
import 'random_id.dart';

void main() {
  var userApi = new UserApi();

  User makeUser(
      {int id, String userName = 'username', String password = 'password'}) {
    return User()
      ..id = id
      ..username = userName
      ..firstName = 'firstname'
      ..lastName = 'lastname'
      ..email = 'email'
      ..password = password
      ..phone = 'phone'
      ..userStatus = 0;
  }

  group('User API with faked client', () {
    test('creates a user', () async {
      final id = newId();
      final username = 'Mally45';
      final newUser = makeUser(id: id, userName: username);

      // use the user api to create a user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user',
        expectedPostRequestBody:
            await userApi.apiClient.serializeAsync(newUser),
        expectedHeaders: {'Content-Type': 'application/json'},
        postResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      await userApi.createUser(newUser);

      // retrieve the same user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/$username',
        getResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      var user = await userApi.getUserByName(username);
      expect(user.id, equals(id));
    });

    test('creates users with list input', () async {
      final firstId = newId();
      final joe = 'Joe';

      final sally = 'Sally';
      final secondId = newId();

      final users = [
        makeUser(id: firstId, userName: joe),
        makeUser(id: secondId, userName: sally),
      ];

      // use the user api to create a list of users
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/createWithList',
        expectedPostRequestBody: await userApi.apiClient.serializeAsync(users),
        expectedHeaders: {'Content-Type': 'application/json'},
        postResponseBody: await userApi.apiClient.serializeAsync(users),
      );
      await userApi.createUsersWithListInput(users);

      // retrieve the users
      userApi.apiClient.client = FakeClient(
        expectedUrl:
            'http://petstore.swagger.io/v2/user/${users.elementAt(0).username}',
        getResponseBody: await userApi.apiClient.serializeAsync(
          users.elementAt(0),
        ),
      );
      final firstUser = await userApi.getUserByName(joe);
      userApi.apiClient.client = FakeClient(
        expectedUrl:
            'http://petstore.swagger.io/v2/user/${users.elementAt(1).username}',
        getResponseBody: await userApi.apiClient.serializeAsync(
          users.elementAt(1),
        ),
      );
      final secondUser = await userApi.getUserByName(sally);
      expect(firstUser.id, equals(firstId));
      expect(secondUser.id, equals(secondId));
    });

    test('updates a user', () async {
      final username = 'Arkjam89';
      final email = 'test@example.com';
      final newUser = makeUser(id: newId(), userName: username);

      // use the user api to create a user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user',
        expectedPostRequestBody:
            await userApi.apiClient.serializeAsync(newUser),
        expectedHeaders: {'Content-Type': 'application/json'},
        postResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      await userApi.createUser(newUser);
      newUser.email = email;

      // use the user api to update the user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/${newUser.username}',
        expectedPutRequestBody: await userApi.apiClient.serializeAsync(newUser),
        expectedHeaders: {'Content-Type': 'application/json'},
        putResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      await userApi.updateUser(username, newUser);

      // retrieve the same user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/${newUser.username}',
        getResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      var foundUser = await userApi.getUserByName(username);
      expect(foundUser.email, equals(email));
    });

    test('deletes a user', () async {
      final username = 'Riddlem325';
      final newUser = makeUser(id: newId(), userName: username);

      // use the user api to create a user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user',
        expectedPostRequestBody:
            await userApi.apiClient.serializeAsync(newUser),
        expectedHeaders: {'Content-Type': 'application/json'},
        postResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      await userApi.createUser(newUser);

      // delete the same user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/${newUser.username}',
        deleteResponseBody: '',
      );
      await userApi.deleteUser(username);

      // try and retrieve the user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user/${newUser.username}',
        throwException: ApiException(400, 'Not found'),
      );
      expect(userApi.getUserByName(username),
          throwsA(TypeMatcher<ApiException>()));
    });

    test('logs a user in', () async {
      final username = 'sgarad625';
      final password = 'lokimoki1';
      final newUser =
          makeUser(id: newId(), userName: username, password: password);

      // use the user api to create a user
      userApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/user',
        expectedPostRequestBody:
            await userApi.apiClient.serializeAsync(newUser),
        expectedHeaders: {'Content-Type': 'application/json'},
        postResponseBody: await userApi.apiClient.serializeAsync(newUser),
      );
      await userApi.createUser(newUser);

      // use the user api to login
      userApi.apiClient.client = FakeClient(
        expectedUrl:
            'http://petstore.swagger.io/v2/user/login?username=${newUser.username}&password=${newUser.password}',
        getResponseBody: 'logged in user session:',
      );
      final result = await userApi.loginUser(username, password);
      expect(result, contains('logged in user session:'));
    });
  });
}
