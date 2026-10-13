//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'user.freezed.dart';
part 'user.g.dart';


@freezed
abstract class User with _$User {
  const factory User({
    @JsonKey(
      name: r'id',
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'username',
      includeIfNull: false,
    )
    String? username,
    @JsonKey(
      name: r'firstName',
      includeIfNull: false,
    )
    String? firstName,
    @JsonKey(
      name: r'lastName',
      includeIfNull: false,
    )
    String? lastName,
    @JsonKey(
      name: r'email',
      includeIfNull: false,
    )
    String? email,
    @JsonKey(
      name: r'password',
      includeIfNull: false,
    )
    String? password,
    @JsonKey(
      name: r'phone',
      includeIfNull: false,
    )
    String? phone,
    /// User Status
    @JsonKey(
      name: r'userStatus',
      includeIfNull: false,
    )
    int? userStatus,
  }) = _User;

  factory User.fromJson(Map<String, dynamic> json) =>
      _$UserFromJson(json);
}

