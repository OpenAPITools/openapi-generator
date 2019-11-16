        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'user.g.dart';

abstract class User implements Built<User, UserBuilder> {

    
        @nullable

    
    @BuiltValueField(wireName: 'id')
    int get id;
    
        @nullable

    
    @BuiltValueField(wireName: 'username')
    String get username;
    
        @nullable

    
    @BuiltValueField(wireName: 'firstName')
    String get firstName;
    
        @nullable

    
    @BuiltValueField(wireName: 'lastName')
    String get lastName;
    
        @nullable

    
    @BuiltValueField(wireName: 'email')
    String get email;
    
        @nullable

    
    @BuiltValueField(wireName: 'password')
    String get password;
    
        @nullable

    
    @BuiltValueField(wireName: 'phone')
    String get phone;
    /* User Status */
        @nullable

    /* User Status */
    @BuiltValueField(wireName: 'userStatus')
    int get userStatus;

    // Boilerplate code needed to wire-up generated code
    User._();

    factory User([updates(UserBuilder b)]) = _$User;
    static Serializer<User> get serializer => _$userSerializer;

}

