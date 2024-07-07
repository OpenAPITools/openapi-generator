import 'dart:math';
import 'dart:typed_data';
import 'package:cross_file/cross_file.dart';
import '_internal.dart';

var $examplesRandom = Random(123);

T? exampleNullable<T>(T Function() result) {
  return $examplesRandom.nextBool() ? result() : null;
}

T exampleEnum<T>(List<T> values) {
  return values.elementAt($examplesRandom.nextInt(values.length));
}

List<T> exampleList<T>(T Function() getItem) {
  return List.generate($examplesRandom.nextInt(10), (_) => getItem());
}
Set<T> exampleSet<T>(T Function() getItem) {
  return exampleList(getItem).toSet();
}
Map<String, T> exampleMap<T>(T Function() getItem) {
  return Map.fromEntries(exampleList(getItem).map((e) => MapEntry(exampleString(), e)));
}

Uint8List exampleUint8List() {
  final len = $examplesRandom.nextInt(50);
  return Uint8List(len )..fillRange(0, len, $examplesRandom.nextInt(255));
}

Object exampleObject() {
  return $examplesRandom.nextBool() ? exampleString() : examplenum();
}
$FreeFormObject example$FreeFormObject() {
  return exampleMap(() => exampleNullable(exampleObject));
}
XFile exampleXFile() {
  final data = exampleUint8List();
  return XFile.fromData(
    data,
    name: exampleString() + ".txt",
    length: data.lengthInBytes,
    mimeType: "text/plain",
  );
}
String exampleString() {
  return 'Random string ${$examplesRandom.nextInt(100)}';
}

int exampleint() {
  return $examplesRandom.nextInt(100);
}

double exampledouble() {
  return $examplesRandom.nextDouble();
}
num examplenum() {
  return $examplesRandom.nextDouble();
}

bool examplebool() {
  return $examplesRandom.nextBool();
}

DateTime exampleDateTime() {
  return DateTime.fromMillisecondsSinceEpoch(
      $examplesRandom.nextInt(4294967296) +
          $examplesRandom.nextInt(4294967296) +
          $examplesRandom.nextInt(4294967296),
      isUtc: true);
}

String? exampleDiscriminator(PartReflection? _partReflection, (ClassReflection<dynamic>, PropertyReflection<dynamic, dynamic>) discriminator) {
  final classReflection = discriminator.$1;
  final discriminatorKey = classReflection.discriminatorKey;

  // if (classReflection != null && discriminatorKey == key) {
  //   final ret = classReflection
  //           .discriminatorMappings.keys.firstOrNull ??
  //       classReflection.discriminatorImplicitMappings.keys.firstOrNull;
  //   if (ret != null) {
  //     return ret;
  //   }
  // }
  return null;
}
