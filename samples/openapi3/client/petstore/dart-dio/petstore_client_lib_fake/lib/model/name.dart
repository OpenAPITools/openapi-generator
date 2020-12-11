import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'name.g.dart';

abstract class Name implements Built<Name, NameBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'name')
    int get name;
    
    @nullable
    @BuiltValueField(wireName: r'snake_case')
    int get snakeCase;
    
    @nullable
    @BuiltValueField(wireName: r'property')
    String get property;
    
    @nullable
    @BuiltValueField(wireName: r'123Number')
    int get n123number;

    // Boilerplate code needed to wire-up generated code
    Name._();

    factory Name([updates(NameBuilder b)]) = _$Name;
    static Serializer<Name> get serializer => _$nameSerializer;
}

