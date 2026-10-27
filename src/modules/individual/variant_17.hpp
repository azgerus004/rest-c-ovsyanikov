#pragma once
#include <array> #include <cmath> #include <iomanip> #include <limits> #include <locale> #include <sstream> #include <string>
namespace TaskVariant {
using namespace std;
struct Input { double base_insurance_rate = 0.0; int total_employees = 0; int hazardous_conditions_count = 0; int incidents_past_year = 0; string parse_error; };
struct Output { double risk_multiplier = 0.0; double final_premium_per_employee = 0.0; double total_annual_insurance_cost = 0.0; };
namespace Detail {
inline bool digit(char c) { return c >= '0' && c <= '9'; }
class Reader { private: const string& text; size_t pos = 0;
public: explicit Reader(const string& body) : text(body) {}
void space() {
    while (pos < text.size() &&
           (text[pos] == ' ' || text[pos] == '\t' ||
            text[pos] == '\r' || text[pos] == '\n')) {
        ++pos;
    }
}

bool take(char c) {
    space();

    if (pos == text.size() || text[pos] != c) {
        return false;
    }

    ++pos;
    return true;
}

bool end() {
    space();
    return pos == text.size();
}

bool key(string& value) {
    if (!take('"')) {
        return false;
    }

    value.clear();

    while (pos < text.size()) {
        unsigned char c =
            static_cast<unsigned char>(text[pos++]);

        if (c == '"') {
            return true;
        }

        if (c < 0x20) {
            return false;
        }

        if (c != '\\') {
            value += static_cast<char>(c);
            continue;
        }

        if (pos == text.size()) {
            return false;
        }

        char escaped = text[pos++];

        switch (escaped) {
        case '"':
        case '\\':
        case '/':
            value += escaped;
            break;

        case 'b':
            value += '\b';
            break;

        case 'f':
            value += '\f';
            break;

        case 'n':
            value += '\n';
            break;

        case 'r':
            value += '\r';
            break;

        case 't':
            value += '\t';
            break;

        case 'u': {
            unsigned code = 0;

            for (int i = 0; i < 4; ++i) {
                if (pos == text.size()) {
                    return false;
                }

                char h = text[pos++];
                unsigned d;

                if (h >= '0' && h <= '9') {
                    d = h - '0';
                } else if (h >= 'a' && h <= 'f') {
                    d = h - 'a' + 10;
                } else if (h >= 'A' && h <= 'F') {
                    d = h - 'A' + 10;
                } else {
                    return false;
                }

                code = code * 16 + d;
            }

            if (code > 127) {
                return false;
            }

            value += static_cast<char>(code);
            break;
        }

        default:
            return false;
        }
    }

    return false;
}

bool number(double& value) {
    space();

    size_t begin = pos;

    if (pos < text.size() && text[pos] == '-') {
        ++pos;
    }

    if (pos == text.size()) {
        return false;
    }

    if (text[pos] == '0') {
        ++pos;
    } else {
        if (text[pos] < '1' || text[pos] > '9') {
            return false;
        }

        while (pos < text.size() && digit(text[pos])) {
            ++pos;
        }
    }

    if (pos < text.size() && text[pos] == '.') {
        ++pos;
        size_t start = pos;

        while (pos < text.size() && digit(text[pos])) {
            ++pos;
        }

        if (pos == start) {
            return false;
        }
    }

    if (pos < text.size() &&
        (text[pos] == 'e' || text[pos] == 'E')) {
        ++pos;

        if (pos < text.size() &&
            (text[pos] == '+' || text[pos] == '-')) {
            ++pos;
        }

        size_t start = pos;

        while (pos < text.size() && digit(text[pos])) {
            ++pos;
        }

        if (pos == start) {
            return false;
        }
    }

    istringstream stream(text.substr(begin, pos - begin));
    stream.
    imbue(locale::classic());
    stream >> value;

    return !stream.fail() &&
           stream.eof() &&
           isfinite(value);
}
};
}
inline Input parse(const string& body) { Input in{};
const array<string, 4> names = {{
    "base_insurance_rate",
    "total_employees",
    "hazardous_conditions_count",
    "incidents_past_year"
}};

array<double, 4> values{};
array<bool, 4> seen{};

Detail::Reader reader(body);

auto fail = [&](const string& message) {
    in.parse_error = message;
    return in;
};

if (!reader.take('{')) {
    return fail("Expected a JSON object");
}

if (!reader.take('}')) {
    do {
        string key;

        if (!reader.key(key) || !reader.take(':')) {
            return fail("Invalid JSON field syntax");
        }

        size_t i = 0;

        while (i < names.size() && names[i] != key) {
            ++i;
        }

        if (i == names.size()) {
            return fail("Unknown input field");
        }

        if (seen[i]) {
            return fail("Duplicate field: " + names[i]);
        }

        if (!reader.number(values[i])) {
            return fail(
                names[i] + " must be a finite JSON number"
            );
        }

        seen[i] = true;

        if (reader.take('}')) {
            break;
        }

        if (!reader.take(',')) {
            return fail("Expected ',' or '}'");
        }
    } while (true);
}

if (!reader.end()) {
    return fail("Unexpected data after JSON object");
}

for (size_t i = 0; i < names.size(); ++i) {
    if (!seen[i]) {
        return fail("Missing required field: " + names[i]);
    }
}

for (size_t i = 1; i < values.size(); ++i) {
    if (trunc(values[i]) != values[i] ||
        values[i] <
            static_cast<double>((numeric_limits<int>::min)()) ||
        values[i] >
            static_cast<double>((numeric_limits<int>::max)())) {
        return fail(
            names[i] + " must be an integer within the int range"
        );
    }
}

in.base_insurance_rate = values[0];
in.total_employees = static_cast<int>(values[1]);
in.hazardous_conditions_count = static_cast<int>(values[2]);
in.incidents_past_year = static_cast<int>(values[3]);

return in;
}
inline string validate(const Input& in) { if (!in.parse_error.empty()) { return in.parse_error; }
if (!isfinite(in.base_insurance_rate) ||
    in.base_insurance_rate <= 0.0) {
    return "base_insurance_rate must be finite and > 0.0";
}

if (in.total_employees < 1) {
    return "total_employees must be >= 1";
}

if (in.hazardous_conditions_count < 0 ||
    in.hazardous_conditions_count > in.total_employees) {
    return "hazardous_conditions_count must be in [0, total_employees]";
}

long long max_incidents =
    static_cast<long long>(in.total_employees) * 3LL;

if (in.incidents_past_year < 0 ||
    static_cast<long long>(in.incidents_past_year) > max_incidents) {
    return "incidents_past_year must be in [0, total_employees * 3]";
}

double multiplier =
    1.0 +
    (
        static_cast<double>(in.hazardous_conditions_count) /
        in.total_employees
    ) * 0.8 +
    (
        static_cast<double>(in.incidents_past_year) /
        in.total_employees
    ) * 1.2;

const double max = (numeric_limits<double>::max)();

if (in.base_insurance_rate > max / multiplier) {
    return "Premium per employee exceeds the double range";
}

double premium = in.base_insurance_rate * multiplier;

if (premium > max / in.total_employees) {
    return "Total annual insurance cost exceeds the double range";
}

return "";
}
inline Output calculate(const Input& in) { Output out{};
out.risk_multiplier =
    1.0 +
    (
        static_cast<double>(in.hazardous_conditions_count) /
        in.total_employees
    ) * 0.8 +
    (
        static_cast<double>(in.incidents_past_year) /
        in.total_employees
    ) * 1.2;

out.final_premium_per_employee =
    in.base_insurance_rate * out.risk_multiplier;

out.total_annual_insurance_cost =
    in.total_employees * out.final_premium_per_employee;

return out;
}
inline string to_json(const Output& out) { ostringstream json; json.

    imbue(locale::classic());
json << setprecision(15)
     << "{\"risk_multiplier\":"
     << out.risk_multiplier
     << ",\"final_premium_per_employee\":"
     << out.final_premium_per_employee
     << ",\"total_annual_insurance_cost\":"
     << out.total_annual_insurance_cost
     << '}';

return json.str();
}
}