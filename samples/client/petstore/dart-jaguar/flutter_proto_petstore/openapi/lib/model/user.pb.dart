///
//  Generated code. Do not modify.
//  source: user.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

class User extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('User')
    ..a<int>(1, 'id', $pb.PbFieldType.O3)
    ..aOS(2, 'username')
    ..aOS(3, 'firstName')
    ..aOS(4, 'lastName')
    ..aOS(5, 'email')
    ..aOS(6, 'password')
    ..aOS(7, 'phone')
    ..a<int>(8, 'userStatus', $pb.PbFieldType.O3)
    ..hasRequiredFields = false
  ;

  User() : super();
  User.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  User.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  User clone() => new User()..mergeFromMessage(this);
  User copyWith(void Function(User) updates) => super.copyWith((message) => updates(message as User));
  $pb.BuilderInfo get info_ => _i;
  static User create() => new User();
  static $pb.PbList<User> createRepeated() => new $pb.PbList<User>();
  static User getDefault() => _defaultInstance ??= create()..freeze();
  static User _defaultInstance;
  static void $checkItem(User v) {
    if (v is! User) $pb.checkItemFailed(v, _i.qualifiedMessageName);
  }

  int get id => $_get(0, 0);
  set id(int v) { $_setSignedInt32(0, v); }
  bool hasId() => $_has(0);
  void clearId() => clearField(1);

  String get username => $_getS(1, '');
  set username(String v) { $_setString(1, v); }
  bool hasUsername() => $_has(1);
  void clearUsername() => clearField(2);

  String get firstName => $_getS(2, '');
  set firstName(String v) { $_setString(2, v); }
  bool hasFirstName() => $_has(2);
  void clearFirstName() => clearField(3);

  String get lastName => $_getS(3, '');
  set lastName(String v) { $_setString(3, v); }
  bool hasLastName() => $_has(3);
  void clearLastName() => clearField(4);

  String get email => $_getS(4, '');
  set email(String v) { $_setString(4, v); }
  bool hasEmail() => $_has(4);
  void clearEmail() => clearField(5);

  String get password => $_getS(5, '');
  set password(String v) { $_setString(5, v); }
  bool hasPassword() => $_has(5);
  void clearPassword() => clearField(6);

  String get phone => $_getS(6, '');
  set phone(String v) { $_setString(6, v); }
  bool hasPhone() => $_has(6);
  void clearPhone() => clearField(7);

  int get userStatus => $_get(7, 0);
  set userStatus(int v) { $_setSignedInt32(7, v); }
  bool hasUserStatus() => $_has(7);
  void clearUserStatus() => clearField(8);
}

