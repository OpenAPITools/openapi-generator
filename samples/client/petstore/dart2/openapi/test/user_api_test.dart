// ref: https://dart.dev/guides/testing
//

part of openapi.tests;


/// tests for UserApi
testUserApi {
  var instance = new UserApi();

  describe('tests for UserApi', () {
    // Create user
    //
    // This can only be done by the logged in user.
    //
    //Future createUser(User body) async 
    it('test createUser', () async {
      // TODO
    });

    // Creates list of users with given input array
    //
    // 
    //
    //Future createUsersWithArrayInput(List<User> body) async 
    it('test createUsersWithArrayInput', () async {
      // TODO
    });

    // Creates list of users with given input array
    //
    // 
    //
    //Future createUsersWithListInput(List<User> body) async 
    it('test createUsersWithListInput', () async {
      // TODO
    });

    // Delete user
    //
    // This can only be done by the logged in user.
    //
    //Future deleteUser(String username) async 
    it('test deleteUser', () async {
      // TODO
    });

    // Get user by user name
    //
    // 
    //
    //Future<User> getUserByName(String username) async 
    it('test getUserByName', () async {
      // TODO
    });

    // Logs user into the system
    //
    // 
    //
    //Future<String> loginUser(String username, String password) async 
    it('test loginUser', () async {
      // TODO
    });

    // Logs out current logged in user session
    //
    // 
    //
    //Future logoutUser() async 
    it('test logoutUser', () async {
      // TODO
    });

    // Updated user
    //
    // This can only be done by the logged in user.
    //
    //Future updateUser(String username, User body) async 
    it('test updateUser', () async {
      // TODO
    });

  });
}
