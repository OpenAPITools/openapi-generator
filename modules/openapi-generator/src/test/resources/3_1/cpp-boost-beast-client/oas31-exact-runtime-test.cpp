#include <boost/json/src.hpp>

#include "oas31_object_array.hpp"
#include "oas31_validator.hpp"
#include "ValidationTypes.h"

#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <exception>
#include <iostream>
#include <stdexcept>
#include <string>

namespace {

void require(bool condition, char const* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

template <typename Function>
void requireThrows(Function&& function, char const* message) {
    try {
        function();
    } catch (std::exception const&) {
        return;
    }
    throw std::runtime_error(message);
}

bool validateNumber(oas31::SchemaEvaluator const& evaluator,
                    std::string const& payload) {
    oas31::ExactJsonValue document = oas31::parseExactJson(payload);
    oas31::ExactInstanceScope scope(document);
    oas31::RawInstance instance(&document.value);
    oas31::ValidationPath path;
    oas31::ValidationContext context;
    return evaluator.validate(0, instance, path, context).success;
}

void testArbitraryExponentsAndMultipleOf() {
    using oas31::ExactNumber;

    ExactNumber const huge = ExactNumber::parseLexeme("1e2147483648");
    ExactNumber const equivalent = ExactNumber::parseLexeme("10e2147483647");
    ExactNumber const tiny = ExactNumber::parseLexeme("1e-2147483648");
    ExactNumber const lessTiny = ExactNumber::parseLexeme("1e-2147483647");
    require(huge == equivalent, "arbitrary exponents must normalize exactly");
    require(tiny < lessTiny, "arbitrary negative exponents must compare exactly");
    require(!tiny.isInteger(), "a tiny decimal must not be an integer");

    require(ExactNumber::parseLexeme("0.3").isMultipleOf(
                ExactNumber::parseLexeme("0.1")),
            "0.3 must be an exact multiple of 0.1");
    require(!ExactNumber::parseLexeme("0.30000000000000004").isMultipleOf(
                ExactNumber::parseLexeme("0.1")),
            "binary-neighbor decimal must not round to a multiple");
    require(huge.isMultipleOf(ExactNumber::parseLexeme("1e2147483647")),
            "huge exponent deltas must not narrow to int32");
    require(!ExactNumber::parseLexeme("1e2147483647").isMultipleOf(huge),
            "negative huge exponent deltas must remain non-integral");
    require(ExactNumber::parseLexeme("1e-2147483648").isMultipleOf(
                ExactNumber::parseLexeme("1e-2147483649")),
            "tiny exponent deltas must be exact");
    require(ExactNumber::parseLexeme("90071992547409931234567890").isMultipleOf(
                ExactNumber::parseLexeme("10")),
            "large mantissas must remain exact");
    require(ExactNumber::parseLexeme("1").isMultipleOf(
                ExactNumber::parseLexeme("0.125")),
            "power-of-two divisors must reduce exactly");
    requireThrows(
            []() {
                (void)ExactNumber::parseLexeme("1").isMultipleOf(ExactNumber());
            },
            "multipleOf zero must be rejected");
}

void testDomLexemesAndEscapedNames() {
    std::string const payload =
            R"JSON({"quo\"te":90071992547409931234567891,"a/b":1e2147483648,"a~b":1e-2147483649,"\u03bb":0.30000000000000004})JSON";
    oas31::ExactJsonValue document = oas31::parseExactJson(payload);
    oas31::ExactInstanceScope scope(document);
    oas31::RawInstance root(&document.value);

    require(root.atMember("quo\"te").asExactNumber()
                    == oas31::ExactNumber::parseLexeme(
                            "90071992547409931234567891"),
            "large integer lexeme must survive DOM parsing");
    require(root.atMember("a/b").asExactNumber()
                    == oas31::ExactNumber::parseLexeme("1e2147483648"),
            "large exponent lexeme must survive DOM fallback parsing");
    require(root.atMember("a~b").asExactNumber()
                    == oas31::ExactNumber::parseLexeme("1e-2147483649"),
            "small exponent lexeme must survive DOM fallback parsing");

    std::string const lambda = u8"\u03bb";
    require(root.atMember({lambda.data(), lambda.size()}).asExactNumber()
                    == oas31::ExactNumber::parseLexeme(
                            "0.30000000000000004"),
            "Unicode member name must resolve its numeric lexeme");

    require(document.lexemes.lexemeAt("/quo\"te") != nullptr,
            "quoted member path must be indexed");
    require(document.lexemes.lexemeAt("/a~1b") != nullptr,
            "slash must use RFC 6901 escaping");
    require(document.lexemes.lexemeAt("/a~0b") != nullptr,
            "tilde must use RFC 6901 escaping");
    require(document.lexemes.lexemeAt("/" + lambda) != nullptr,
            "Unicode path bytes must be preserved");
}

void testDeepEnumAndConstNumbers() {
    std::string const storedPayload =
            R"JSON([{"nested":[90071992547409931234567891,1e2147483648]}])JSON";
    std::string const matchingPayload =
            R"JSON({"nested":[90071992547409931234567891,10e2147483647]})JSON";
    std::string const differentPayload =
            R"JSON({"nested":[90071992547409931234567892,1e2147483648]})JSON";

    oas31::ExactJsonValue stored = oas31::parseExactJson(storedPayload);
    oas31::ExactJsonValue matching = oas31::parseExactJson(matchingPayload);
    oas31::ExactJsonValue different = oas31::parseExactJson(differentPayload);

    {
        oas31::ExactInstanceScope scope(matching);
        oas31::RawInstance instance(&matching.value);
        require(oas31::deepInstanceEqual(
                        instance, stored.value.as_array()[0],
                        &stored.lexemes, "/0"),
                "deep enum/const numbers must compare by exact lexeme value");
    }
    {
        oas31::ExactInstanceScope scope(different);
        oas31::RawInstance instance(&different.value);
        require(!oas31::deepInstanceEqual(
                        instance, stored.value.as_array()[0],
                        &stored.lexemes, "/0"),
                "deep equality must reject a distinct large integer");
    }
}

void testSharedCompositionEvaluator() {
    oas31::SchemaResourceRegistry registry;
    registry.nodes.resize(3);
    registry.nodes[0].oneOfChildren = {1, 2};
    registry.nodes[1].hasMultipleOf = true;
    registry.nodes[1].multipleOf = oas31::ExactNumber::parseLexeme("0.1");
    registry.nodes[2].hasMultipleOf = true;
    registry.nodes[2].multipleOf = oas31::ExactNumber::parseLexeme("0.2");

    oas31::SchemaEvaluator const evaluator(registry);
    require(validateNumber(evaluator, "0.3"),
            "oneOf must accept exactly one exact numeric match");
    require(!validateNumber(evaluator, "0.4"),
            "oneOf must reject two exact numeric matches");
    require(!validateNumber(evaluator, "0.25"),
            "oneOf must reject zero exact numeric matches");

    oas31::SchemaResourceRegistry recursiveRegistry;
    recursiveRegistry.nodes.resize(1);
    recursiveRegistry.nodes[0].applicator = oas31::ApplicatorKind::ref;
    recursiveRegistry.nodes[0].children.push_back(0);
    oas31::SchemaEvaluator const recursiveEvaluator(recursiveRegistry);
    require(validateNumber(recursiveEvaluator, "1e2147483648"),
            "recursive shared evaluation must terminate");
}

void testNumericConversionBoundaries() {
    std::int64_t signedValue = 0;
    double const signedUpper = std::ldexp(1.0, 63);
    boost::json::value const tooLargeSigned(signedUpper);
    require(!tryGetMathematicalInteger(tooLargeSigned, signedValue),
            "2^63 must not convert to int64");

    boost::json::value const largestDoubleBelowSignedUpper(
            std::nextafter(signedUpper, 0.0));
    require(tryGetMathematicalInteger(
                    largestDoubleBelowSignedUpper, signedValue),
            "the largest integral double below 2^63 must convert to int64");

    boost::json::value const signedLower(-signedUpper);
    require(tryGetMathematicalInteger(signedLower, signedValue)
                    && signedValue == (std::numeric_limits<std::int64_t>::min)(),
            "-2^63 must convert to int64 exactly");

    std::uint64_t unsignedValue = 0;
    boost::json::value const tooLargeUnsigned(std::ldexp(1.0, 64));
    require(!tryGetMathematicalInteger(tooLargeUnsigned, unsignedValue),
            "2^64 must not convert to uint64");
    requireThrows(
            [&]() { (void)convertJsonNumber<std::int64_t>(tooLargeSigned); },
            "out-of-range floating-to-integer conversion must throw");

    boost::json::value const negativeNumber(-1.5);
    require(convertJsonNumber<double>(negativeNumber) == -1.5,
            "negative floating values must use lowest(), not min(), as the bound");
    requireThrows(
            [&]() { (void)convertJsonNumber<std::int64_t>(negativeNumber); },
            "fractional values must not truncate into integer destinations");
}

void testFractionalBoundsAroundZero() {
    oas31::ExactNumber const zero;
    oas31::ExactNumber const half = oas31::ExactNumber::parseLexeme("0.5");
    oas31::ExactNumber const negativeHalf =
            oas31::ExactNumber::parseLexeme("-0.5");
    require(zero.compare(half) < 0 && half.compare(zero) > 0,
            "zero must sort below a positive fraction");
    require(negativeHalf.compare(zero) < 0 && zero.compare(negativeHalf) > 0,
            "zero must sort above a negative fraction");

    oas31::SchemaResourceRegistry registry;
    registry.nodes.resize(1);
    oas31::SchemaEvaluator const evaluator(registry);

    registry.nodes[0].hasMinimum = true;
    registry.nodes[0].minimum = half;
    require(!validateNumber(evaluator, "0"),
            "minimum 0.5 must reject zero");

    registry.nodes[0].hasMinimum = false;
    registry.nodes[0].hasMaximum = true;
    registry.nodes[0].maximum = half;
    require(validateNumber(evaluator, "0"),
            "maximum 0.5 must accept zero");

    registry.nodes[0].hasMaximum = false;
    registry.nodes[0].hasExclusiveMinimum = true;
    registry.nodes[0].exclusiveMinimum =
            oas31::ExactNumber::parseLexeme("0.25");
    require(!validateNumber(evaluator, "0"),
            "exclusiveMinimum 0.25 must reject zero");
}

oas31::Annotation const* annotationFor(
        oas31::ValidationContext const& context,
        std::string const& keyword) {
    for (oas31::Annotation const& annotation : context.annotations.all()) {
        if (annotation.keyword == keyword) return &annotation;
    }
    return nullptr;
}

void testAnnotationPayloadsLocationsAndRollback() {
    oas31::SchemaResourceRegistry registry;
    registry.nodes.resize(5);
    registry.nodes[0].anyOfChildren = {1, 3};

    registry.nodes[1].hasObjectSchema = true;
    registry.nodes[1].properties.push_back({"a/b", 2});
    registry.nodes[1].additionalProperties =
            oas31::AdditionalPropertiesKind::reject;
    registry.nodes[1].dynamicResource = 7;
    registry.nodes[2].annTitle = "\"discarded\"";
    registry.nodes[2].schemaPath =
            "#/components/schemas/Failed/properties/a~1b";
    registry.nodes[2].absSchemaUri =
            "https://example.test/schema#/components/schemas/Failed/properties/a~1b";

    registry.nodes[3].hasObjectSchema = true;
    registry.nodes[3].properties.push_back({"a/b", 4});
    registry.nodes[3].dynamicResource = 8;
    registry.nodes[4].annDescription = "\"kept\"";
    registry.nodes[4].annExamplesJson = "[1,\"x\"]";
    registry.nodes[4].annDeprecatedJson = "false";
    registry.nodes[4].annContentSchemaJson = "{\"type\":\"integer\"}";
    registry.nodes[4].annExtras.push_back({"x/a", "{\"k\":true}"});
    registry.nodes[4].schemaPath =
            "#/components/schemas/Kept/properties/a~1b";
    registry.nodes[4].absSchemaUri =
            "https://example.test/schema#/components/schemas/Kept/properties/a~1b";

    oas31::ExactJsonValue document =
            oas31::parseExactJson(R"JSON({"a/b":1,"extra":2})JSON");
    oas31::ExactInstanceScope scope(document);
    oas31::RawInstance instance(&document.value);
    oas31::ValidationPath path;
    oas31::ValidationContext context;
    context.dynamicScope.push_back(42);
    context.currentValidationRes = 42;
    context.curProps().insert("preexisting");

    oas31::SchemaEvaluator const evaluator(registry);
    require(evaluator.validate(0, instance, path, context).success,
            "a successful anyOf branch must validate");
    require(annotationFor(context, "title") == nullptr,
            "annotations from a failed branch must roll back");
    require(context.dynamicScope.size() == 1 && context.dynamicScope[0] == 42,
            "dynamic scope must be restored after branch evaluation");
    require(context.currentValidationRes == 42,
            "validation resource must be restored after branch evaluation");
    require(context.activeEvaluations.empty(),
            "active evaluation guards must be restored after validation");
    require(context.curProps().count("preexisting") == 1,
            "preexisting evaluated coverage must survive transactions");

    oas31::Annotation const* description = annotationFor(context, "description");
    require(description != nullptr && description->value == "\"kept\"",
            "string annotations must retain complete JSON text");
    require(description->instancePath == "/a~1b",
            "annotation instance locations must use RFC 6901 escaping");
    require(description->schemaPath
                    == "#/components/schemas/Kept/properties/a~1b/description",
            "annotation schema locations must include the keyword pointer");
    require(description->absSchemaUri
                    == "https://example.test/schema#/components/schemas/Kept/properties/a~1b/description",
            "annotation absolute locations must retain the resource URI");

    oas31::Annotation const* examples = annotationFor(context, "examples");
    require(examples != nullptr && examples->value == "[1,\"x\"]",
            "examples must be one JSON array annotation value");
    oas31::Annotation const* contentSchema =
            annotationFor(context, "contentSchema");
    require(contentSchema != nullptr
                    && contentSchema->value == "{\"type\":\"integer\"}",
            "contentSchema must preserve its schema JSON value");
    oas31::Annotation const* extra = annotationFor(context, "x/a");
    require(extra != nullptr
                    && extra->schemaPath
                            == "#/components/schemas/Kept/properties/a~1b/x~1a",
            "unknown annotation keyword locations must escape pointer tokens");

    oas31::SchemaResourceRegistry invalidRegistry;
    invalidRegistry.nodes.resize(3);
    invalidRegistry.nodes[0].allOfChildren = {1, 2};
    invalidRegistry.nodes[1].annTitle = "\"transient\"";
    invalidRegistry.nodes[1].schemaPath = "#/allOf/0";
    invalidRegistry.nodes[1].absSchemaUri = "urn:test#/allOf/0";
    invalidRegistry.nodes[2].booleanValue = oas31::BooleanValue::false_;
    oas31::SchemaEvaluator const invalidEvaluator(invalidRegistry);
    oas31::ValidationContext invalidContext;
    require(!invalidEvaluator.validate(0, instance, path, invalidContext).success,
            "the final failing allOf branch must reject the parent");
    require(invalidContext.annotations.all().empty(),
            "a failing parent schema must roll back earlier child annotations");
}

} // namespace

int main() {
    try {
        testArbitraryExponentsAndMultipleOf();
        testDomLexemesAndEscapedNames();
        testDeepEnumAndConstNumbers();
        testSharedCompositionEvaluator();
        testNumericConversionBoundaries();
        testFractionalBoundsAroundZero();
        testAnnotationPayloadsLocationsAndRollback();
        std::cout << "oas31 exact runtime tests passed\n";
        return EXIT_SUCCESS;
    } catch (std::exception const& exception) {
        std::cerr << exception.what() << '\n';
        return EXIT_FAILURE;
    }
}
