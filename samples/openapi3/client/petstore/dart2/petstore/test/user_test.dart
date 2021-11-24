@Skip('Needs real petstore')
import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var userApi = new UserApi();

  User makeUser({int id, String userName = 'username', String password = 'password'}) {
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

  group('User API with live client', () {
    test('creates a user', () async {
      var id = newId();
      var username = 'Mally45';
      await userApi.createUser(makeUser(id: id, userName: username));
      var user = await userApi.getUserByName(username);
      expect(user.id, equals(id));
    });

    test('creates users with list input', () async {
      var firstId = newId();
      var joe = 'Joe';

      var sally = 'Sally';
      var secondId = newId();

      var users = [
        makeUser(id: firstId, userName: joe),
        makeUser(id: secondId, userName: sally),
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
      var user = makeUser(id: newId(), userName: username);

      await userApi.createUser(user);
      user.email = email;
      await userApi.updateUser(username, user);
      var foundUser = await userApi.getUserByName(username);
      expect(foundUser.email, equals(email));
    });

    test('deletes a user', () async {
      var username = 'Riddlem325';
      await userApi.createUser(makeUser(id: newId(), userName: username));
      await userApi.deleteUser(username);
      expect(userApi.getUserByName(username), throwsA(TypeMatcher<ApiException>()));
    });

    test('logs a user in', () async {
      var username = 'sgarad625';
      var password = 'lokimoki1';
      var user = makeUser(id: newId(), userName: username, password: password);

      await userApi.createUser(user);
      var result = await userApi.loginUser(username, password);
      expect(result, contains('logged in user session:'));
    });
  });
}
