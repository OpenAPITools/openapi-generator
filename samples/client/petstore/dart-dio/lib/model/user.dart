        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'user.g.dart';

abstract class User implements Built<User, UserBuilder> {

    
        @nullable
    @BuiltValueField(wireName: r'id')
    int get id;
    
        @nullable
    @BuiltValueField(wireName: r'username')
    String get username;
    
        @nullable
    @BuiltValueField(wireName: r'firstName')
    String get firstName;
    
        @nullable
    @BuiltValueField(wireName: r'lastName')
    String get lastName;
    
        @nullable
    @BuiltValueField(wireName: r'email')
    String get email;
    
        @nullable
    @BuiltValueField(wireName: r'password')
    String get password;
    
        @nullable
    @BuiltValueField(wireName: r'phone')
    String get phone;
    /* User Status */
        @nullable
    @BuiltValueField(wireName: r'userStatus')
    int get userStatus;

    // Boilerplate code needed to wire-up generated code
    User._();

    factory User([updates(UserBuilder b)]) = _$User;
    static Serializer<User> get serializer => _$userSerializer;
}

