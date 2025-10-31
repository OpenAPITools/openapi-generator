//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// User
    ///
    /// Properties:
        /// * [id] 
        /// * [username] 
        /// * [firstName] 
        /// * [lastName] 
        /// * [email] 
        /// * [password] 
        /// * [phone] 
        /// * [userStatus] - User Status

        @freezed
        class User with _$User {
        const User._();
        
        const factory User({
                    @JsonKey(name: r'id') 
    int?
 id,
                    @JsonKey(name: r'username') 
    String?
 username,
                    @JsonKey(name: r'firstName') 
    String?
 firstName,
                    @JsonKey(name: r'lastName') 
    String?
 lastName,
                    @JsonKey(name: r'email') 
    String?
 email,
                    @JsonKey(name: r'password') 
    String?
 password,
                    @JsonKey(name: r'phone') 
    String?
 phone,
                        /// User Status
            @JsonKey(name: r'userStatus') 
    int?
 userStatus,
        }) = _User;


        factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);






}



