<?php

require_once 'BaseApiTest.php';

class PetApiTest extends BaseApiTest {

  // Choose a random username for new pet insertion
  public static $randomUsername1;
  public static $randomUsername2;
  public static $randomUsername3;
  public static $randomUsername4;
  public static $randomUsername5;

  public static function setUpBeforeClass() {
    $letters = 'ABCDEFGHIJKLMNOPQRSTUVXYZ';
    $letter_arr = str_split($letters);
    shuffle($letter_arr);
    self::$randomUsername1 = implode('', $letter_arr);
    print "Username1 is " . self::$randomUsername1 . "\n\n";
    shuffle($letter_arr);
    self::$randomUsername2 = implode('', $letter_arr);
    print "Username2 is " . self::$randomUsername2 . "\n\n";
    shuffle($letter_arr);
    self::$randomUsername3 = implode('', $letter_arr);
    print "Username3 is " . self::$randomUsername3 . "\n\n";
    shuffle($letter_arr);
    self::$randomUsername4 = implode('', $letter_arr);
    print "Username4 is " . self::$randomUsername4 . "\n\n";
    shuffle($letter_arr);
    self::$randomUsername5 = implode('', $letter_arr);
    print "Username5 is " . self::$randomUsername5 . "\n\n";
  }

  public function testCreateUsersWithArrayInput() {

    $user = new User();

    $user->id = rand(10000, 100000);
    $user->lastName = rand(10000, 100000);
    $user->username = self::$randomUsername1;
    $user->phone = rand(10000, 100000);
    $user->email = rand(10000, 100000);
    $user->userStatus = rand(10000, 100000);
    $user->firstName = rand(10000, 100000);
    $user->password = rand(10000, 100000);

    $otherUser = new User();

    $otherUser->id = rand(10000, 100000);
    $otherUser->lastName = rand(10000, 100000);
    $otherUser->username = self::$randomUsername2;
    $otherUser->phone = rand(10000, 100000);
    $otherUser->email = rand(10000, 100000);
    $otherUser->userStatus = rand(10000, 100000);
    $otherUser->firstName = rand(10000, 100000);
    $otherUser->password = rand(10000, 100000);

    $users = array($user, $otherUser);
    $res = $this->userApi->createUsersWithArrayInput($users);

    $new_user = $this->userApi->getUserByName(self::$randomUsername1);

    $this->assertEquals($new_user->id, $user->id);
    $this->assertEquals($new_user->lastName, $user->lastName);
    $this->assertEquals($new_user->username, $user->username);
    $this->assertEquals($new_user->phone, $user->phone);
    $this->assertEquals($new_user->email, $user->email);
    $this->assertEquals($new_user->userStatus, $user->userStatus);
    $this->assertEquals($new_user->firstName, $user->firstName);
    $this->assertEquals($new_user->password, $user->password);

    $new_user = $this->userApi->getUserByName(self::$randomUsername2);

    $this->assertEquals($new_user->id, $otherUser->id);
    $this->assertEquals($new_user->lastName, $otherUser->lastName);
    $this->assertEquals($new_user->username, $otherUser->username);
    $this->assertEquals($new_user->phone, $otherUser->phone);
    $this->assertEquals($new_user->email, $otherUser->email);
    $this->assertEquals($new_user->userStatus, $otherUser->userStatus);
    $this->assertEquals($new_user->firstName, $otherUser->firstName);
    $this->assertEquals($new_user->password, $otherUser->password);

  }

  public function testCreateUsersWithListInput() {

    $user = new User();

    $user->id = rand(10000, 100000);
    $user->lastName = rand(10000, 100000);
    $user->username = self::$randomUsername4;
    $user->phone = rand(10000, 100000);
    $user->email = rand(10000, 100000);
    $user->userStatus = rand(10000, 100000);
    $user->firstName = rand(10000, 100000);
    $user->password = rand(10000, 100000);

    $otherUser = new User();

    $otherUser->id = rand(10000, 100000);
    $otherUser->lastName = rand(10000, 100000);
    $otherUser->username = self::$randomUsername5;
    $otherUser->phone = rand(10000, 100000);
    $otherUser->email = rand(10000, 100000);
    $otherUser->userStatus = rand(10000, 100000);
    $otherUser->firstName = rand(10000, 100000);
    $otherUser->password = rand(10000, 100000);

    $users = array($user, $otherUser);
    $res = $this->userApi->createUsersWithArrayInput($users);

    $new_user = $this->userApi->getUserByName(self::$randomUsername4);

    $this->assertEquals($new_user->id, $user->id);
    $this->assertEquals($new_user->lastName, $user->lastName);
    $this->assertEquals($new_user->username, $user->username);
    $this->assertEquals($new_user->phone, $user->phone);
    $this->assertEquals($new_user->email, $user->email);
    $this->assertEquals($new_user->userStatus, $user->userStatus);
    $this->assertEquals($new_user->firstName, $user->firstName);
    $this->assertEquals($new_user->password, $user->password);

    $new_user = $this->userApi->getUserByName(self::$randomUsername5);

    $this->assertEquals($new_user->id, $otherUser->id);
    $this->assertEquals($new_user->lastName, $otherUser->lastName);
    $this->assertEquals($new_user->username, $otherUser->username);
    $this->assertEquals($new_user->phone, $otherUser->phone);
    $this->assertEquals($new_user->email, $otherUser->email);
    $this->assertEquals($new_user->userStatus, $otherUser->userStatus);
    $this->assertEquals($new_user->firstName, $otherUser->firstName);
    $this->assertEquals($new_user->password, $otherUser->password);

  }

  public function testCreateUser() {

    $user = new User();

    $user->id = rand(10000, 100000);
    $user->lastName = rand(10000, 100000);
    $user->username = self::$randomUsername3;
    $user->phone = rand(10000, 100000);
    $user->email = rand(10000, 100000);
    $user->userStatus = rand(10000, 100000);
    $user->firstName = rand(10000, 100000);
    $user->password = rand(10000, 100000);

    $res = $this->userApi->createUser($user);

    $new_user = $this->userApi->getUserByName(self::$randomUsername3);

    $this->assertEquals($new_user->id, $user->id);
    $this->assertEquals($new_user->lastName, $user->lastName);
    $this->assertEquals($new_user->username, $user->username);
    $this->assertEquals($new_user->phone, $user->phone);
    $this->assertEquals($new_user->email, $user->email);
    $this->assertEquals($new_user->userStatus, $user->userStatus);
    $this->assertEquals($new_user->firstName, $user->firstName);
    $this->assertEquals($new_user->password, $user->password);

  }

  public function testUpdateUser() {

    $user = $this->userApi->getUserByName(self::$randomUsername1);

    $user->lastName = rand(10000, 100000);
    $user->phone = rand(10000, 100000);
    $user->email = rand(10000, 100000);
    $user->userStatus = rand(10000, 100000);
    $user->firstName = rand(10000, 100000);
    $user->password = rand(10000, 100000);

    $res = $this->userApi->updateUser(self::$randomUsername1, $user);

    $updated_user = $this->userApi->getUserByName(self::$randomUsername1);

    $this->assertEquals($updated_user->lastName, $user->lastName);
    $this->assertEquals($updated_user->username, $user->username);
    $this->assertEquals($updated_user->phone, $user->phone);
    $this->assertEquals($updated_user->email, $user->email);
    $this->assertEquals($updated_user->userStatus, $user->userStatus);
    $this->assertEquals($updated_user->firstName, $user->firstName);
    $this->assertEquals($updated_user->password, $user->password);

  }

  public function testDeleteUser() {

    $res = $this->userApi->deleteUser(self::$randomUsername1);

    $deleted_user = $this->userApi->getUserByName(self::$randomUsername1);

    $this->assertEquals($deleted_user, null);

  }

  public function testLoginUser() {

    $res = $this->userApi->loginUser("anyusername", "anypassword");

    $this->assertEquals(substr($res, 0, 23), "logged in user session:");

  }

  public function testLogoutUser() {

    $res = $this->userApi->logoutUser();

    // We just want to make sure there are no errors in this test.
    // To verify you are getting back a 200, you might want to add
    // something like this to Swagger.php callAPI():
    // print "Response for call to $resourcePath : ";
    // print_r($data);

  }

}
?>