// ============================================================================
// oas31_exact_number.hpp - exact JSON Number domain (ADR D1).
//
// A finite base-10 arbitrary-precision decimal: value = mantissa * 10^exponent10.
// Fully independent of Boost.JSON `double`. Keeps 1 == 1.0 == 1e0 under ==/compare.
// multipleOf is an exact divmod with zero remainder — never floating division.
//
// HEADER-ONLY. Built under -Werror with g++ -std=c++17.
// ============================================================================
#ifndef OAS31_EXACT_NUMBER_HPP_
#define OAS31_EXACT_NUMBER_HPP_

#include <boost/multiprecision/cpp_int.hpp>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <limits>
#include <locale>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>

namespace oas31 {

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

    ExactNumber(Integer mantissa, Integer exponent10)
        : mantissa_(std::move(mantissa)), exponent10_(std::move(exponent10)) {
        normalize();
    }

    /// Parse a raw JSON numeric lexeme (e.g. "-1.5e+3", "0.0001", "-0",
    /// "1e2147483648") exactly. Does not pass through binary floating point.
    static ExactNumber parseLexeme(std::string const& lexeme);

    static ExactNumber fromInt(std::int64_t v) { return ExactNumber(Integer(v), 0); }
    static ExactNumber fromUint(std::uint64_t v) { return ExactNumber(Integer(v), 0); }

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

// ---------------------------------------------------------------------------
// Implementation (header-only)
// ---------------------------------------------------------------------------

inline ExactNumber::Integer ExactNumber::ipow10(std::size_t n) {
    Integer result = 1;
    Integer base = 10;
    while (n != 0) {
        if ((n & 1u) != 0u) result *= base;
        n >>= 1u;
        if (n != 0) base *= base;
    }
    return result;
}

inline ExactNumber::Integer ExactNumber::gcd(Integer a, Integer b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
        Integer const remainder = a % b;
        a = std::move(b);
        b = remainder;
    }
    return a;
}

inline std::size_t ExactNumber::boundedDistance(Integer const& distance) {
    if (distance < 0 || distance > Integer(kMaxLexemeLength)) {
        throw std::length_error(
            "ExactNumber: decimal expansion exceeds implementation limit");
    }
    return distance.convert_to<std::size_t>();
}

inline std::size_t ExactNumber::decimalDigits(Integer const& positive) {
    return positive.str().size();
}

inline void ExactNumber::normalize() {
    if (mantissa_ == 0) {
        exponent10_ = 0;
        return;
    }
    Integer const magnitude = mantissa_ < 0 ? -mantissa_ : mantissa_;
    std::string const digits = magnitude.str();
    std::size_t trailingZeros = 0;
    while (trailingZeros < digits.size()
            && digits[digits.size() - trailingZeros - 1] == '0') {
        ++trailingZeros;
    }
    if (trailingZeros != 0) {
        mantissa_ /= ipow10(trailingZeros);
        exponent10_ += Integer(trailingZeros);
    }
}

inline ExactNumber ExactNumber::parseLexeme(std::string const& s) {
    if (s.size() > kMaxLexemeLength) {
        throw std::length_error(
            "ExactNumber::parseLexeme: numeric token exceeds implementation limit");
    }

    std::size_t i = 0;
    bool negative = false;
    if (i < s.size() && s[i] == '-') {
        negative = true;
        ++i;
    }

    std::string digits;
    if (i >= s.size()) {
        throw std::domain_error("ExactNumber::parseLexeme: missing integer digit");
    }
    if (s[i] == '0') {
        digits.push_back(s[i++]);
        if (i < s.size() && s[i] >= '0' && s[i] <= '9') {
            throw std::domain_error("ExactNumber::parseLexeme: leading zero");
        }
    } else if (s[i] >= '1' && s[i] <= '9') {
        do {
            digits.push_back(s[i++]);
        } while (i < s.size() && s[i] >= '0' && s[i] <= '9');
    } else {
        throw std::domain_error("ExactNumber::parseLexeme: missing integer digit");
    }

    std::size_t fractionDigits = 0;
    if (i < s.size() && s[i] == '.') {
        ++i;
        std::size_t const fractionStart = i;
        while (i < s.size() && s[i] >= '0' && s[i] <= '9') {
            digits.push_back(s[i++]);
            ++fractionDigits;
        }
        if (i == fractionStart) {
            throw std::domain_error("ExactNumber::parseLexeme: missing fraction digit");
        }
    }

    Integer explicitExponent = 0;
    if (i < s.size() && (s[i] == 'e' || s[i] == 'E')) {
        ++i;
        bool exponentNegative = false;
        if (i < s.size() && (s[i] == '+' || s[i] == '-')) {
            exponentNegative = s[i] == '-';
            ++i;
        }
        std::size_t const exponentStart = i;
        while (i < s.size() && s[i] >= '0' && s[i] <= '9') {
            explicitExponent *= 10;
            explicitExponent += static_cast<unsigned>(s[i] - '0');
            ++i;
        }
        if (i == exponentStart) {
            throw std::domain_error("ExactNumber::parseLexeme: missing exponent digit");
        }
        if (exponentNegative) explicitExponent = -explicitExponent;
    }
    if (i != s.size()) {
        throw std::domain_error("ExactNumber::parseLexeme: trailing characters after lexeme");
    }

    std::size_t const firstNonZero = digits.find_first_not_of('0');
    if (firstNonZero == std::string::npos) return ExactNumber();

    std::size_t trailingZeros = 0;
    while (digits[digits.size() - trailingZeros - 1] == '0') ++trailingZeros;
    digits.resize(digits.size() - trailingZeros);

    Integer mantissa = 0;
    for (char digit : digits) {
        mantissa *= 10;
        mantissa += static_cast<unsigned>(digit - '0');
    }
    if (negative) mantissa = -mantissa;

    Integer exponent = explicitExponent - Integer(fractionDigits)
            + Integer(trailingZeros);
    return ExactNumber(std::move(mantissa), std::move(exponent));
}

inline ExactNumber ExactNumber::fromDouble(double v) {
    if (!std::isfinite(v)) {
        throw std::domain_error("ExactNumber::fromDouble: non-finite value");
    }
    std::ostringstream text;
    text.imbue(std::locale::classic());
    text << std::setprecision(std::numeric_limits<double>::max_digits10) << v;
    return parseLexeme(text.str());
}

inline int ExactNumber::compare(ExactNumber const& other) const {
    if (isZero() && other.isZero()) return 0;
    if (isZero()) return other.mantissa_ < 0 ? 1 : -1;
    if (other.isZero()) return mantissa_ < 0 ? -1 : 1;
    bool const negative = mantissa_ < 0;
    bool const otherNegative = other.mantissa_ < 0;
    if (negative != otherNegative) return negative ? -1 : 1;

    Integer left = negative ? -mantissa_ : mantissa_;
    Integer right = otherNegative ? -other.mantissa_ : other.mantissa_;
    std::size_t const leftDigits = decimalDigits(left);
    std::size_t const rightDigits = decimalDigits(right);
    Integer const leftMagnitude = exponent10_ + Integer(leftDigits);
    Integer const rightMagnitude = other.exponent10_ + Integer(rightDigits);

    int result = 0;
    if (leftMagnitude < rightMagnitude) {
        result = -1;
    } else if (leftMagnitude > rightMagnitude) {
        result = 1;
    } else {
        if (leftDigits < rightDigits) {
            left *= ipow10(rightDigits - leftDigits);
        } else if (rightDigits < leftDigits) {
            right *= ipow10(leftDigits - rightDigits);
        }
        if (left < right) result = -1;
        else if (left > right) result = 1;
    }
    return negative ? -result : result;
}

inline ExactNumber ExactNumber::add(ExactNumber const& other) const {
    Integer const commonExponent = exponent10_ < other.exponent10_
            ? exponent10_ : other.exponent10_;
    std::size_t const leftShift = boundedDistance(exponent10_ - commonExponent);
    std::size_t const rightShift = boundedDistance(other.exponent10_ - commonExponent);
    Integer const left = mantissa_ * ipow10(leftShift);
    Integer const right = other.mantissa_ * ipow10(rightShift);
    return ExactNumber(left + right, commonExponent);
}

inline ExactNumber ExactNumber::mul(ExactNumber const& other) const {
    return ExactNumber(mantissa_ * other.mantissa_,
                       exponent10_ + other.exponent10_);
}

inline bool ExactNumber::isInteger() const {
    return isZero() || exponent10_ >= 0;
}

inline bool ExactNumber::isMultipleOf(ExactNumber const& divisor) const {
    if (divisor.isZero()) {
        throw std::domain_error("ExactNumber::isMultipleOf: division by zero");
    }
    if (isZero()) return true;

    Integer const exponentDelta = exponent10_ - divisor.exponent10_;
    // Canonical non-zero mantissas are not divisible by 10, so shifting this
    // value to the right of the divisor can never produce an integer quotient.
    if (exponentDelta < 0) return false;

    Integer numerator = mantissa_ < 0 ? -mantissa_ : mantissa_;
    Integer denominator = divisor.mantissa_ < 0
            ? -divisor.mantissa_ : divisor.mantissa_;
    denominator /= gcd(numerator, denominator);

    std::size_t twos = 0;
    while ((denominator % 2) == 0) {
        denominator /= 2;
        ++twos;
    }
    std::size_t fives = 0;
    while ((denominator % 5) == 0) {
        denominator /= 5;
        ++fives;
    }
    if (denominator != 1) return false;
    return exponentDelta >= Integer((std::max)(twos, fives));
}

inline void ExactNumber::divmod(ExactNumber const& divisor,
                                ExactNumber& quotient,
                                ExactNumber& remainder) const {
    if (divisor.isZero()) {
        throw std::domain_error("ExactNumber::divmod: division by zero");
    }
    Integer const commonExponent = exponent10_ < divisor.exponent10_
            ? exponent10_ : divisor.exponent10_;
    std::size_t const numeratorShift =
        boundedDistance(exponent10_ - commonExponent);
    std::size_t const denominatorShift =
        boundedDistance(divisor.exponent10_ - commonExponent);
    Integer const numerator = mantissa_ * ipow10(numeratorShift);
    Integer const denominator = divisor.mantissa_ * ipow10(denominatorShift);
    Integer const q = numerator / denominator;
    Integer const r = numerator % denominator;
    quotient = ExactNumber(q, 0);
    remainder = ExactNumber(r, commonExponent);
}

inline std::string ExactNumber::toString() const {
    if (isZero()) return "0";
    std::string result = mantissa_.str();
    if (exponent10_ != 0) {
        result += "e";
        if (exponent10_ > 0) result += "+";
        result += exponent10_.str();
    }
    return result;
}

// JSON Schema requires multipleOf to be strictly positive.
inline bool isPositiveMultipleOf(ExactNumber const& m) {
    return !m.isZero() && m.mantissa() > 0;
}

} // namespace oas31

#endif // OAS31_EXACT_NUMBER_HPP_
