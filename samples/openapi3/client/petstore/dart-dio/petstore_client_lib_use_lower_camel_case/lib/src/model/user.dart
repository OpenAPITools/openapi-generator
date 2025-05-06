//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class User {
  /// Returns a new [User] instance.
  User({

     this.id,

     this.username,

     this.firstname,

     this.lastname,

     this.email,

     this.password,

     this.phone,

     this.userstatus,
  });

  @JsonKey(
    
    name: r'id',
    required: false,
    includeIfNull: false,
  )


  final int? id;



  @JsonKey(
    
    name: r'username',
    required: false,
    includeIfNull: false,
  )


  final String? username;



  @JsonKey(
    
    name: r'firstName',
    required: false,
    includeIfNull: false,
  )


  final String? firstname;



  @JsonKey(
    
    name: r'lastName',
    required: false,
    includeIfNull: false,
  )


  final String? lastname;



  @JsonKey(
    
    name: r'email',
    required: false,
    includeIfNull: false,
  )


  final String? email;



  @JsonKey(
    
    name: r'password',
    required: false,
    includeIfNull: false,
  )


  final String? password;



  @JsonKey(
    
    name: r'phone',
    required: false,
    includeIfNull: false,
  )


  final String? phone;



      /// User Status
  @JsonKey(
    
    name: r'userStatus',
    required: false,
    includeIfNull: false,
  )


  final int? userstatus;





    @override
    bool operator ==(Object other) => identical(this, other) || other is User &&
      other.id == id &&
      other.username == username &&
      other.firstname == firstname &&
      other.lastname == lastname &&
      other.email == email &&
      other.password == password &&
      other.phone == phone &&
      other.userstatus == userstatus;

    @override
    int get hashCode =>
        id.hashCode +
        username.hashCode +
        firstname.hashCode +
        lastname.hashCode +
        email.hashCode +
        password.hashCode +
        phone.hashCode +
        userstatus.hashCode;

  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);

  Map<String, dynamic> toJson() => _$UserToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

