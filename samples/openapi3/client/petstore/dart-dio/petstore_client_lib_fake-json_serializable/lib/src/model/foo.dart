//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'foo.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Foo {
  /// Returns a new [Foo] instance.
  Foo({

     this.bar = 'bar',
  });

  @JsonKey(
    defaultValue: 'bar',
    name: r'bar',
    required: false,
    includeIfNull: false
  )


  final String? bar;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Foo &&
     other.bar == bar;

  @override
  int get hashCode =>
    bar.hashCode;

  factory Foo.fromJson(Map<String, dynamic> json) => _$FooFromJson(json);

  Map<String, dynamic> toJson() => _$FooToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

