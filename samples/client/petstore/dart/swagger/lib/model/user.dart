part of swagger.api;

@Entity()
class User {
  
  @Property(name: 'id')
  int id = null;
  

  @Property(name: 'username')
  String username = null;
  

  @Property(name: 'firstName')
  String firstName = null;
  

  @Property(name: 'lastName')
  String lastName = null;
  

  @Property(name: 'email')
  String email = null;
  

  @Property(name: 'password')
  String password = null;
  

  @Property(name: 'phone')
  String phone = null;
  
/* User Status */
  @Property(name: 'userStatus')
  int userStatus = null;
  
  User();

  @override
  String toString()  {
    return 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus, ]';
  }
}

