import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'user.jser.dart';

class User {
  
  final int id;
  
  final String username;
  
  final String firstName;
  
  final String lastName;
  
  final String email;
  
  final String password;
  
  final String phone;
   /* User Status */
  final int userStatus;
  

  User(
    

{
     this.id = null,  
     this.username = null,  
     this.firstName = null,  
     this.lastName = null,  
     this.email = null,  
     this.password = null,  
     this.phone = null,  
     this.userStatus = null 
    
    }
  );

  @override
  String toString() {
    return 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus, ]';
  }
}

@GenSerializer()
class UserSerializer extends Serializer<User> with _$UserSerializer {

}
