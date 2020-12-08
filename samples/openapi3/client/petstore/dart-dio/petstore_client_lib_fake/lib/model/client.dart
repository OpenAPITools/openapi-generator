import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'client.g.dart';

abstract class Client implements Built<Client, ClientBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'client')
    String get client;

    // Boilerplate code needed to wire-up generated code
    Client._();

    factory Client([updates(ClientBuilder b)]) = _$Client;
    static Serializer<Client> get serializer => _$clientSerializer;
}

