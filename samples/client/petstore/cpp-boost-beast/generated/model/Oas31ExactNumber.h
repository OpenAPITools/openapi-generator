// ============================================================================
// Oas31ExactNumber.h - exact JSON Number domain (ADR D1).
//
// A finite base-10 arbitrary-precision decimal: value = mantissa * 10^exponent10.
// Fully independent of Boost.JSON `double`. Keeps 1 == 1.0 == 1e0 under ==/compare.
// multipleOf is an exact divmod with zero remainder — never floating division.
//
// Interface-only; implementation is compiled from Oas31ExactNumber.cpp.
// ============================================================================
#ifndef ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_EXACT_NUMBER_H_
#define ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_EXACT_NUMBER_H_

#include <boost/multiprecision/cpp_int.hpp>

#include <cstddef>
#include <cstdint>
#include <string>

namespace org::openapitools::client::model::detail::schema_validation {

/// Finite base-10 arbitrary-precision decimal: value = mantissa * 10^exponent10.
/// Both components are canonical: zero is (0, 0), and a non-zero mantissa has
/// no trailing decimal zero. The exponent is arbitrary precision so valid JSON
/// numbers such as 1e2147483648 do not overflow an implementation integer.
class ExactNumber {
public:
    using Integer = boost::multiprecision::cpp_int;

    // Explicit implementation limit permitted by RFC 8259 section 9. It caps
    // work by input size, never by numeric magnitude (a short huge exponent is
    // still accepted).
    static constexpr std::size_t kMaxLexemeLength = 4096;

    ExactNumber() = default;

    ExactNumber(Integer mantissa, Integer exponent10);

    /// Parse a raw JSON numeric lexeme (e.g. "-1.5e+3", "0.0001", "-0",
    /// "1e2147483648") exactly. Does not pass through binary floating point.
    static ExactNumber parseLexeme(std::string const& lexeme);

    static ExactNumber fromInt(std::int64_t v);
    static ExactNumber fromUint(std::uint64_t v);

    /// Lossy fallback used only when the original JSON lexeme is unavailable.
    /// Rejects non-finite doubles instead of invoking an undefined conversion.
    static ExactNumber fromDouble(double v);

    Integer const& mantissa() const { return mantissa_; }
    Integer const& exponent10() const { return exponent10_; }

    int compare(ExactNumber const& other) const;

    bool isZero() const { return mantissa_ == 0; }

    ExactNumber add(ExactNumber const& other) const;
    ExactNumber mul(ExactNumber const& other) const;

    /// Legacy exact divmod for callers that need quotient/remainder values.
    /// Operations requiring more than kMaxLexemeLength expanded digits fail
    /// with length_error rather than attempting an unbounded allocation.
    void divmod(ExactNumber const& divisor, ExactNumber& quotient,
                ExactNumber& remainder) const;

    /// Exact divisibility without exponent expansion, including exponents with
    /// arbitrarily many magnitude bits. A zero divisor is rejected.
    bool isMultipleOf(ExactNumber const& divisor) const;

    bool isInteger() const;

    bool operator==(ExactNumber const& o) const { return compare(o) == 0; }
    bool operator!=(ExactNumber const& o) const { return compare(o) != 0; }
    bool operator<(ExactNumber const& o) const { return compare(o) < 0; }

    std::string toString() const;

private:
    static Integer ipow10(std::size_t n);
    static Integer gcd(Integer a, Integer b);
    static std::size_t boundedDistance(Integer const& distance);
    static std::size_t decimalDigits(Integer const& positive);
    void normalize();

    Integer mantissa_;
    Integer exponent10_;
};

/// JSON Schema requires multipleOf to be strictly positive.
bool isPositiveMultipleOf(ExactNumber const& value);

} // namespace org::openapitools::client::model::detail::schema_validation

#endif // ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_EXACT_NUMBER_H_
