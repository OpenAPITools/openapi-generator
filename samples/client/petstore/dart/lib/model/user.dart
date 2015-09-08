part of api;


@Entity()
class User {
  
  int id = null;
  
  
  String username = null;
  
  
  String firstName = null;
  
  
  String lastName = null;
  
  
  String email = null;
  
  
  String password = null;
  
  
  String phone = null;
  
  /* User Status */
  int userStatus = null;
  
  
  User();

  @override
  String toString()  {
    return 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus, ]';
  }

}

