// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'user.reflection.dart';
part 'user.serialization.dart';


//class defination

///
mixin UserMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<String> get username;
  UndefinedWrapper<String> get firstName;
  UndefinedWrapper<String> get lastName;
  UndefinedWrapper<String> get email;
  UndefinedWrapper<String> get password;
  UndefinedWrapper<String> get phone;
  UndefinedWrapper<int> get userStatus;
  UndefinedWrapper<$OpenApiObjectMixin> get objectWithNoDeclaredProps;
  UndefinedWrapper<$OpenApiObjectMixin?> get objectWithNoDeclaredPropsNullable;
  UndefinedWrapper<Object?> get anyTypeProp;
  UndefinedWrapper<Object?> get anyTypePropNullable;


}

///
class User with
$OpenApiObjectMixin,


UserMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<String> username;
  @override
  UndefinedWrapper<String> firstName;
  @override
  UndefinedWrapper<String> lastName;
  @override
  UndefinedWrapper<String> email;
  @override
  UndefinedWrapper<String> password;
  @override
  UndefinedWrapper<String> phone;
  @override
  UndefinedWrapper<int> userStatus;
  @override
  UndefinedWrapper<$OpenApiObjectMixin> objectWithNoDeclaredProps;
  @override
  UndefinedWrapper<$OpenApiObjectMixin?> objectWithNoDeclaredPropsNullable;
  @override
  UndefinedWrapper<Object?> anyTypeProp;
  @override
  UndefinedWrapper<Object?> anyTypePropNullable;





  User.$all({
    required this.id,
    required this.username,
    required this.firstName,
    required this.lastName,
    required this.email,
    required this.password,
    required this.phone,
    required this.userStatus,
    required this.objectWithNoDeclaredProps,
    required this.objectWithNoDeclaredPropsNullable,
    required this.anyTypeProp,
    required this.anyTypePropNullable,
    
    
  });

  User({
    this.id = const UndefinedWrapper.undefined(),
    this.username = const UndefinedWrapper.undefined(),
    this.firstName = const UndefinedWrapper.undefined(),
    this.lastName = const UndefinedWrapper.undefined(),
    this.email = const UndefinedWrapper.undefined(),
    this.password = const UndefinedWrapper.undefined(),
    this.phone = const UndefinedWrapper.undefined(),
    this.userStatus = const UndefinedWrapper.undefined(),
    this.objectWithNoDeclaredProps = const UndefinedWrapper.undefined(),
    this.objectWithNoDeclaredPropsNullable = const UndefinedWrapper.undefined(),
    this.anyTypeProp = const UndefinedWrapper.undefined(),
    this.anyTypePropNullable = const UndefinedWrapper.undefined(),
    
    
  });
}




