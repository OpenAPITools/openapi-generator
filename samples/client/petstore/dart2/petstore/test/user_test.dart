@TestOn('browser')

import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var userApi = new UserApi();

  group('User API ', () {
    test('creates a user', () async {
      var id = newId();
      var username = 'Mally45';
      await userApi.createUser(new User()
        ..id = id
        ..username = username);
      var user = await userApi.getUserByName(username);
      expect(user.id, equals(id));
    });

    test('creates users with list input', () async {
      var firstId = newId();
      var joe = 'Joe';

      var sally = 'Sally';
      var secondId = newId();

      var users = [
        new User()
          ..id = firstId
          ..username = joe,
        new User()
          ..id = secondId
          ..username = sally
      ];

      await userApi.createUsersWithListInput(users);
      var firstUser = await userApi.getUserByName(joe);
      var secondUser = await userApi.getUserByName(sally);
      expect(firstUser.id, equals(firstId));
      expect(secondUser.id, equals(secondId));
    });

    test('updates a user', () async {
      var username = 'Arkjam89';
      var email = 'test@example.com';
      var user = new User()
        ..id = newId()
        ..username = username;

      await userApi.createUser(user);
      user.email = email;
      await userApi.updateUser(username, user);
      var foundUser = await userApi.getUserByName(username);
      expect(foundUser.email, equals(email));
    });

    test('deletes a user', () async {
      var username = 'Riddlem325';
      await userApi.createUser(new User()
        ..id = newId()
        ..username = username);
      await userApi.deleteUser(username);
      expect(userApi.getUserByName(username), throwsA(TypeMatcher<ApiException>()));
    });

    test('logs a user in', () async {
      var username = 'sgarad625';
      var password = 'lokimoki1';
      var user = new User()
        ..id = newId()
        ..username = username
        ..password = password;

      await userApi.createUser(user);
      var result = await userApi.loginUser(username, password);
      expect(result, contains('logged in user session:'));
    });
  });
}
