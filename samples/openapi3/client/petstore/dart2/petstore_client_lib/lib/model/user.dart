//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

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


  int? id;

  String? username;

  String? firstName;

  String? lastName;

  String? email;

  String? password;

  String? phone;

  /// User Status
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

  @override
  String toString() => 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[r'id'] = id;
    }
    if (username != null) {
      json[r'username'] = username;
    }
    if (firstName != null) {
      json[r'firstName'] = firstName;
    }
    if (lastName != null) {
      json[r'lastName'] = lastName;
    }
    if (email != null) {
      json[r'email'] = email;
    }
    if (password != null) {
      json[r'password'] = password;
    }
    if (phone != null) {
      json[r'phone'] = phone;
    }
    if (userStatus != null) {
      json[r'userStatus'] = userStatus;
    }
    return json;
  }

  /// Returns a new [User] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static User fromJson(Map<String, dynamic> json) => User(
        id: json[r'id'] as int,
        username: json[r'username'] as String,
        firstName: json[r'firstName'] as String,
        lastName: json[r'lastName'] as String,
        email: json[r'email'] as String,
        password: json[r'password'] as String,
        phone: json[r'phone'] as String,
        userStatus: json[r'userStatus'] as int,
    );

  static List<User> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<User>((i) => User.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <User>[];

  static Map<String, User> mapFromJson(dynamic json) {
    final map = <String, User>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = User.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of User-objects as value to a dart map
  static Map<String, List<User>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<User>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = User.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

