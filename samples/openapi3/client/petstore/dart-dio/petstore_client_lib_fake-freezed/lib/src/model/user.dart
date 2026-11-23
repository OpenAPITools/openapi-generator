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
      required: false,
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'username',
      required: false,
      includeIfNull: false,
    )
    String? username,
    @JsonKey(
      name: r'firstName',
      required: false,
      includeIfNull: false,
    )
    String? firstName,
    @JsonKey(
      name: r'lastName',
      required: false,
      includeIfNull: false,
    )
    String? lastName,
    @JsonKey(
      name: r'email',
      required: false,
      includeIfNull: false,
    )
    String? email,
    @JsonKey(
      name: r'password',
      required: false,
      includeIfNull: false,
    )
    String? password,
    @JsonKey(
      name: r'phone',
      required: false,
      includeIfNull: false,
    )
    String? phone,
    /// User Status
    @JsonKey(
      name: r'userStatus',
      required: false,
      includeIfNull: false,
    )
    int? userStatus,
  }) = _User;

  factory User.fromJson(Map<String, dynamic> json) =>
      _$UserFromJson(json);
}

