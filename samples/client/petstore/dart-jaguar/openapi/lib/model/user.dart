import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'user.jser.dart';

class User {
  
  @Alias('id')
  final int id;
  
  @Alias('username')
  final String username;
  
  @Alias('firstName')
  final String firstName;
  
  @Alias('lastName')
  final String lastName;
  
  @Alias('email')
  final String email;
  
  @Alias('password')
  final String password;
  
  @Alias('phone')
  final String phone;
   /* User Status */
  @Alias('userStatus')
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

@GenSerializer(nullableFields: true)
class UserSerializer extends Serializer<User> with _$UserSerializer {

}

