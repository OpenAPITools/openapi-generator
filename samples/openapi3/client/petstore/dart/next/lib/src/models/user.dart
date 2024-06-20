// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'user.reflection.dart';
part 'user.serialization.dart';


/// UserMixin
///
/// Properties:
/// * [id] 
/// * [username] 
/// * [firstName] 
/// * [lastName] 
/// * [email] 
/// * [password] 
/// * [phone] 
/// * [userStatus] - User Status
/// * [objectWithNoDeclaredProps] - test code generation for objects Value must be a map of strings to values. It cannot be the 'null' value.
/// * [objectWithNoDeclaredPropsNullable] - test code generation for nullable objects. Value must be a map of strings to values or the 'null' value.
/// * [anyTypeProp] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389
/// * [anyTypePropNullable] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The 'nullable' attribute does not change the allowed values.
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

/// User
///
/// Properties:
/// * [id] 
/// * [username] 
/// * [firstName] 
/// * [lastName] 
/// * [email] 
/// * [password] 
/// * [phone] 
/// * [userStatus] - User Status
/// * [objectWithNoDeclaredProps] - test code generation for objects Value must be a map of strings to values. It cannot be the 'null' value.
/// * [objectWithNoDeclaredPropsNullable] - test code generation for nullable objects. Value must be a map of strings to values or the 'null' value.
/// * [anyTypeProp] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389
/// * [anyTypePropNullable] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The 'nullable' attribute does not change the allowed values.
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




