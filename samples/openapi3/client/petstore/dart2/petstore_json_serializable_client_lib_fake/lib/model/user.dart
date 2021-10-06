//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class User {
  /// Returns a new [User] instance.
  User({
    this.id,
    this.username,
    this.firstName,
    this.lastName,
    this.email,
    this.password,
    this.phone,
    this.userStatus,
  });


  @JsonKey(
    name: r'id',
    required: false,
  )
  int? id;

  @JsonKey(
    name: r'username',
    required: false,
  )
  String? username;

  @JsonKey(
    name: r'firstName',
    required: false,
  )
  String? firstName;

  @JsonKey(
    name: r'lastName',
    required: false,
  )
  String? lastName;

  @JsonKey(
    name: r'email',
    required: false,
  )
  String? email;

  @JsonKey(
    name: r'password',
    required: false,
  )
  String? password;

  @JsonKey(
    name: r'phone',
    required: false,
  )
  String? phone;

  /// User Status
  @JsonKey(
    name: r'userStatus',
    required: false,
  )
  int? userStatus;

  @override
  bool operator ==(Object other) => identical(this, other) || other is User &&
     other.id == id &&
     other.username == username &&
     other.firstName == firstName &&
     other.lastName == lastName &&
     other.email == email &&
     other.password == password &&
     other.phone == phone &&
     other.userStatus == userStatus;

  @override
  int get hashCode =>
    id.hashCode +
    username.hashCode +
    firstName.hashCode +
    lastName.hashCode +
    email.hashCode +
    password.hashCode +
    phone.hashCode +
    userStatus.hashCode;

  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);

  Map<String, dynamic> toJson() => _$UserToJson(this);

  @override
  String toString() => toJson().toString();
}

