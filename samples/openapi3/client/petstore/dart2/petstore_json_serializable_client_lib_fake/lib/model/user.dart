//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
    nullable: false,
    name: r'id',
    required: false,
  )
  int id;

  @JsonKey(
    nullable: false,
    name: r'username',
    required: false,
  )
  String username;

  @JsonKey(
    nullable: false,
    name: r'firstName',
    required: false,
  )
  String firstName;

  @JsonKey(
    nullable: false,
    name: r'lastName',
    required: false,
  )
  String lastName;

  @JsonKey(
    nullable: false,
    name: r'email',
    required: false,
  )
  String email;

  @JsonKey(
    nullable: false,
    name: r'password',
    required: false,
  )
  String password;

  @JsonKey(
    nullable: false,
    name: r'phone',
    required: false,
  )
  String phone;

      /// User Status
  @JsonKey(
    nullable: false,
    name: r'userStatus',
    required: false,
  )
  int userStatus;

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
    (id == null ? 0 : id.hashCode) +
    (username == null ? 0 : username.hashCode) +
    (firstName == null ? 0 : firstName.hashCode) +
    (lastName == null ? 0 : lastName.hashCode) +
    (email == null ? 0 : email.hashCode) +
    (password == null ? 0 : password.hashCode) +
    (phone == null ? 0 : phone.hashCode) +
    (userStatus == null ? 0 : userStatus.hashCode);

  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);

  Map<String, dynamic> toJson() => _$UserToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

