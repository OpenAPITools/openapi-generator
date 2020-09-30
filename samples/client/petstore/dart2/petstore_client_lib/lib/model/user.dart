//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [User].
abstract class UserStrings {
  const UserStrings._();

  static const id_ = "id";
  static const username_ = "username";
  static const firstName_ = "firstName";
  static const lastName_ = "lastName";
  static const email_ = "email";
  static const password_ = "password";
  static const phone_ = "phone";
  static const userStatus_ = "userStatus";
}

class User {
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
    id.hashCode +
    username.hashCode +
    firstName.hashCode +
    lastName.hashCode +
    email.hashCode +
    password.hashCode +
    phone.hashCode +
    userStatus.hashCode;

  @override
  String toString() => _toString("");

  User.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    id = json[UserStrings.id_];
    username = json[UserStrings.username_];
    firstName = json[UserStrings.firstName_];
    lastName = json[UserStrings.lastName_];
    email = json[UserStrings.email_];
    password = json[UserStrings.password_];
    phone = json[UserStrings.phone_];
    userStatus = json[UserStrings.userStatus_];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[UserStrings.id_] = id;
    }
    if (username != null) {
      json[UserStrings.username_] = username;
    }
    if (firstName != null) {
      json[UserStrings.firstName_] = firstName;
    }
    if (lastName != null) {
      json[UserStrings.lastName_] = lastName;
    }
    if (email != null) {
      json[UserStrings.email_] = email;
    }
    if (password != null) {
      json[UserStrings.password_] = password;
    }
    if (phone != null) {
      json[UserStrings.phone_] = phone;
    }
    if (userStatus != null) {
      json[UserStrings.userStatus_] = userStatus;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("User=[");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.id_);
    sb.write(": ");
    sb.write(id);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.username_);
    sb.write(": ");
    sb.write(username);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.firstName_);
    sb.write(": ");
    sb.write(firstName);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.lastName_);
    sb.write(": ");
    sb.write(lastName);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.email_);
    sb.write(": ");
    sb.write(email);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.password_);
    sb.write(": ");
    sb.write(password);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.phone_);
    sb.write(": ");
    sb.write(phone);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(UserStrings.userStatus_);
    sb.write(": ");
    sb.write(userStatus);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<User> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <User>[]
      : json.map((v) => User.fromJson(v)).toList(growable: true == growable);

  static Map<String, User> mapFromJson(Map<String, dynamic> json) {
    final map = <String, User>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = User.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of User-objects as value to a dart map
  static Map<String, List<User>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<User>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = User.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

