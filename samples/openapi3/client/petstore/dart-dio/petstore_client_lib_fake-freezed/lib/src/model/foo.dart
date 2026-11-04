//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'foo.freezed.dart';
part 'foo.g.dart';


@freezed
abstract class Foo with _$Foo {
  const factory Foo({
    @JsonKey(
      name: r'bar',
      required: false,
      includeIfNull: false,
    )
  
    @Default('bar') String bar,
  }) = _Foo;

  factory Foo.fromJson(Map<String, dynamic> json) =>
      _$FooFromJson(json);
}

