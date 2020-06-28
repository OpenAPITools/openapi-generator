import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'user.jser.dart';

class User {
  
  @Alias('id', isNullable: false,  )
  final int id;
  
  @Alias('username', isNullable: false,  )
  final String username;
  
  @Alias('firstName', isNullable: false,  )
  final String firstName;
  
  @Alias('lastName', isNullable: false,  )
  final String lastName;
  
  @Alias('email', isNullable: false,  )
  final String email;
  
  @Alias('password', isNullable: false,  )
  final String password;
  
  @Alias('phone', isNullable: false,  )
  final String phone;
   /* User Status */
  @Alias('userStatus', isNullable: false,  )
  final int userStatus;
  

  User(
      

{
     this.id
     this.username
     this.firstName
     this.lastName
     this.email
     this.password
     this.phone
     this.userStatus
    
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

