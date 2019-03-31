// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'user.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$UserSerializer implements Serializer<User> {
  @override
  Map<String, dynamic> toMap(User model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'id', model.id);
    setMapValue(ret, 'username', model.username);
    setMapValue(ret, 'firstName', model.firstName);
    setMapValue(ret, 'lastName', model.lastName);
    setMapValue(ret, 'email', model.email);
    setMapValue(ret, 'password', model.password);
    setMapValue(ret, 'phone', model.phone);
    setMapValue(ret, 'userStatus', model.userStatus);
    return ret;
  }

  @override
  User fromMap(Map map) {
    if (map == null) return null;
    final obj = new User(
        id: map['id'] as int ?? getJserDefault('id'),
        username: map['username'] as String ?? getJserDefault('username'),
        firstName: map['firstName'] as String ?? getJserDefault('firstName'),
        lastName: map['lastName'] as String ?? getJserDefault('lastName'),
        email: map['email'] as String ?? getJserDefault('email'),
        password: map['password'] as String ?? getJserDefault('password'),
        phone: map['phone'] as String ?? getJserDefault('phone'),
        userStatus: map['userStatus'] as int ?? getJserDefault('userStatus'));
    return obj;
  }
}
