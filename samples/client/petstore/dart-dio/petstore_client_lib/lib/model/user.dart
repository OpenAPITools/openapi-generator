//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'user.g.dart';

abstract class User implements Built<User, UserBuilder> {

    @BuiltValueField(wireName: r'id')
    int get id;

    @BuiltValueField(wireName: r'username')
    String get username;

    @BuiltValueField(wireName: r'firstName')
    String get firstName;

    @BuiltValueField(wireName: r'lastName')
    String get lastName;

    @BuiltValueField(wireName: r'email')
    String get email;

    @BuiltValueField(wireName: r'password')
    String get password;

    @BuiltValueField(wireName: r'phone')
    String get phone;

    /// User Status
    @BuiltValueField(wireName: r'userStatus')
    int get userStatus;

    // Boilerplate code needed to wire-up generated code
    User._();

    static void _initializeBuilder(UserBuilder b) => b;

    factory User([void updates(UserBuilder b)]) = _$User;
    static Serializer<User> get serializer => _$userSerializer;
}

