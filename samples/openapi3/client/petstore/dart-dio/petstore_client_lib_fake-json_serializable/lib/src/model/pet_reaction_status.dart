//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';


enum PetReactionStatus {
      @JsonValue(r'liked')
      liked(r'liked'),
      @JsonValue(r'disliked')
      disliked(r'disliked'),
      @JsonValue(r'barked')
      barked(r'barked'),
      @JsonValue(r'unknown_default_open_api')
      unknownDefaultOpenApi(r'unknown_default_open_api');

  const PetReactionStatus(this.value);

  final String value;

  @override
  String toString() => value;
}
