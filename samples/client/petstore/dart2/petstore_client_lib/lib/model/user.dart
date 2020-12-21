//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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

  int id;

  String username;

  String firstName;

  String lastName;

  String email;

  String password;

  String phone;

  /// User Status
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
  /// [json] if it's non-null, null if [json] is null.
  static User fromJson(Map<String, dynamic> json) => json == null
    ? null
    : User(
        id: json[r'id'],
        username: json[r'username'],
        firstName: json[r'firstName'],
        lastName: json[r'lastName'],
        email: json[r'email'],
        password: json[r'password'],
        phone: json[r'phone'],
        userStatus: json[r'userStatus'],
    );

  static List<User> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <User>[]
      : json.map((dynamic value) => User.fromJson(value)).toList(growable: true == growable);

  static Map<String, User> mapFromJson(Map<String, dynamic> json) {
    final map = <String, User>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = User.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of User-objects as value to a dart map
  static Map<String, List<User>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<User>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = User.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

