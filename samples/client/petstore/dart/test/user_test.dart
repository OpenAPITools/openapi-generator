part of tests;

testUserApi() {
  var userApi = new UserApi();

  describe('User API ', () {

    it('creates a user', () async {
      var id = 67567;
      var username = 'Mally45';
      await userApi.createUser(new User()..id = id..username = username);
      var user = await userApi.getUserByName(username);
      expect(user.id).toEqual(id);
    });

    it('creates users with list input', () async {
      var firstId = 46226;
      var joe ='Joe';

      var sally = 'Sally';
      var secondId = 95239;

      var users = [ new User()..id = firstId..username = joe,
                    new User()..id = secondId..username = sally];

      await userApi.createUsersWithListInput(users);
      var firstUser = await userApi.getUserByName(joe);
      var secondUser = await userApi.getUserByName(sally);
      expect(firstUser.id).toEqual(firstId);
      expect(secondUser.id).toEqual(secondId);
    });

    it('updates a user', () async {
      var username ='Arkjam89';
      var email = 'test@example.com';
      var user = new User()..id = 733356..username = username;

      await userApi.createUser(user);
      user.email = email;
      await userApi.updateUser(username,user);
      var foundUser = await userApi.getUserByName(username);
      expect(foundUser.email).toEqual(email);
    });

    it('deletes a user', () async {
      var username ='Riddlem325';
      await userApi.createUser(new User()..id = 1231114..username = username);
      await userApi.deleteUser(username);
      expect(userApi.getUserByName(username)).toThrowWith(anInstanceOf: ApiException);
    });

    it('logs a user in', () async {
      var username ='sgarad625';
      var password = 'lokimoki1';
      var user = new User()..id = 733356..username = username..password = password;

      await userApi.createUser(user);
      var result = await userApi.loginUser(username, password);
      expect(result).toContain('logged in user session:');
    });

  });
}