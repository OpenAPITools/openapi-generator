part of openapi.api;

class User {
  
  int id = null;
  
  String username = null;
  
  String firstName = null;
  
  String lastName = null;
  
  String email = null;
  
  String password = null;
  
  String phone = null;
  /* User Status */
  int userStatus = null;
  User();

  @override
  String toString() {
    return 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus, ]';
  }

  User.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    if (json['id'] == null) {
      id = null;
    } else {
          id = json['id'];
    }
    if (json['username'] == null) {
      username = null;
    } else {
          username = json['username'];
    }
    if (json['firstName'] == null) {
      firstName = null;
    } else {
          firstName = json['firstName'];
    }
    if (json['lastName'] == null) {
      lastName = null;
    } else {
          lastName = json['lastName'];
    }
    if (json['email'] == null) {
      email = null;
    } else {
          email = json['email'];
    }
    if (json['password'] == null) {
      password = null;
    } else {
          password = json['password'];
    }
    if (json['phone'] == null) {
      phone = null;
    } else {
          phone = json['phone'];
    }
    if (json['userStatus'] == null) {
      userStatus = null;
    } else {
          userStatus = json['userStatus'];
    }
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'username': username,
      'firstName': firstName,
      'lastName': lastName,
      'email': email,
      'password': password,
      'phone': phone,
      'userStatus': userStatus
    };
  }

  static List<User> listFromJson(List<dynamic> json) {
    return json == null ? new List<User>() : json.map((value) => new User.fromJson(value)).toList();
  }

  static Map<String, User> mapFromJson(Map<String, dynamic> json) {
    var map = new Map<String, User>();
    if (json != null && json.length > 0) {
      json.forEach((String key, dynamic value) => map[key] = new User.fromJson(value));
    }
    return map;
  }
}

