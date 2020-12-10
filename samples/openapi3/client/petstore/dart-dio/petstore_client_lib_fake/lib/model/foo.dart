import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo.g.dart';

abstract class Foo implements Built<Foo, FooBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'bar')
    String get bar;

    // Boilerplate code needed to wire-up generated code
    Foo._();

    factory Foo([updates(FooBuilder b)]) = _$Foo;
    static Serializer<Foo> get serializer => _$fooSerializer;
}

