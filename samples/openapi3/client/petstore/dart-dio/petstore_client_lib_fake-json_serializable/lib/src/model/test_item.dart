//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/test_enum.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'test_item.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class TestItem {
  /// Returns a new [TestItem] instance.
  TestItem({

    required  this.test,

     this.testEmum,
  });

  @JsonKey(
    
    name: r'test',
    required: true,
    includeIfNull: false,
  )


  final int test;



  @JsonKey(
    
    name: r'testEmum',
    required: false,
    includeIfNull: false,
  unknownEnumValue: TestEnum.unknownDefaultOpenApi,
  )


  final TestEnum? testEmum;





    @override
    bool operator ==(Object other) => identical(this, other) || other is TestItem &&
      other.test == test &&
      other.testEmum == testEmum;

    @override
    int get hashCode =>
        test.hashCode +
        testEmum.hashCode;

  factory TestItem.fromJson(Map<String, dynamic> json) => _$TestItemFromJson(json);

  Map<String, dynamic> toJson() => _$TestItemToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

