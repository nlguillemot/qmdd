#include <string>
#include <fstream>
#include <vector>
#include <map>
#include <memory>
#include <array>
#include <unordered_set>
#include <cstdio>
#include <cctype>
#include <cstdint>
#include <cassert>

#define SHOW_INSTRS

enum class gate_opcode : int
{
    toffoli,
    fredkin,
    pauli_y,
    pauli_z,
    sqrtnot,
    inv_sqrtnot,
    hadamard,
    rotate_pi_by_4,
    inv_rotate_pi_by_4
};

struct program_spec
{
    int num_variables;
    std::vector<std::string> variable_names;
    std::vector<int> variable_input_list_index;
    std::vector<int> variable_output_list_index;
    std::vector<int> variable_constant_input;
    std::map<std::string, int> variable_name_to_id;

    int num_inputs;
    std::vector<int> input_variable_ids;

    int num_outputs;
    std::vector<int> output_variable_ids;

    // gate instruction encoding:
    // 1. gate opcode
    // 2. operand count
    // 3. operand variable IDs
    std::vector<int> gate_stream;
};

program_spec parse(const char* txt)
{
    static auto is_eol = [](const char* s)
    {
        return *s == '\0' || *s == '\n';
    };

    static auto is_eol_or_comment = [](const char* s)
    {
        return is_eol(s) || *s == '#';
    };

    static auto opt_ws = [](const char* s)
    {
        while (!is_eol(s) && std::isspace(*s))
        {
            s++;
        }

        return s;
    };

    static auto opt_insensitive = [](const char* kw, const char* s)
    {
        size_t kwlen = strlen(kw);
        for (size_t i = 0; i < kwlen && !is_eol(s); i++, s++)
        {
            if (std::toupper(*s) != std::toupper(kw[i]))
            {
                return (const char*)NULL;
            }
        }

        if (!*s || std::isspace(*s))
        {
            return s;
        }
        else
        {
            return (const char*)NULL;
        }
    };

    static auto opt_sensitive = [](const char* kw, const char* s)
    {
        size_t kwlen = strlen(kw);
        for (size_t i = 0; i < kwlen && !is_eol(s); i++, s++)
        {
            if (*s != kw[i])
            {
                return (const char*)NULL;
            }
        }

        if (!*s || std::isspace(*s))
        {
            return s;
        }
        else
        {
            return (const char*)NULL;
        }
    };

    static auto accept_paramcount = [](const char* s, int* pcnt)
    {
        if (!std::isdigit(*s) || *s == '0')
        {
            throw std::runtime_error("expected parameter count");
        }

        int paramcount = 0;
        while (std::isdigit(*s))
        {
            paramcount = paramcount * 10;
            paramcount += *s - '0';

            if (paramcount > SHRT_MAX)
            {
                throw std::runtime_error("parameter count too big");
            }

            s++;
        }

        if (*s && !std::isspace(*s))
        {
            throw std::runtime_error("expected parameter count");
        }

        if (pcnt) *pcnt = paramcount;
        return s;
    };

    static auto accept_list = [](const char* s, auto callback)
    {
        while (!is_eol_or_comment(s))
        {
            const char* s_end = s;
            while (!is_eol_or_comment(s_end) && *s_end != ',')
            {
                s_end++;
            }

            if (s == s_end)
            {
                throw std::runtime_error("missing variable name");
            }

            if (std::isspace(*s) || std::isspace(*(s_end - 1)))
            {
                throw std::runtime_error("whitespace at beginning or end of variable name");
            }

            callback(s, s_end);

            s = s_end;
            if (*s == ',')
            {
                s++;
            }
        }

        return s;
    };

    static auto next_line = [](const char* s)
    {
        if (!is_eol_or_comment(s))
        {
            throw std::runtime_error("expected eol or comment");
        }

        while (!is_eol(s))
        {
            s++;
        }

        if (*s == '\n')
        {
            s++;
        }

        return s;
    };

    static auto parse_spec = [](const char* s)
    {
        program_spec spec;

        spec.num_variables = 0;
        spec.num_inputs = 0;
        spec.num_outputs = 0;

        int line = 0;
        const char* linestart = s;

        bool has_variable_listing = false;
        bool has_input_variable_listing = false;
        bool has_output_variable_listing = false;
        bool has_constant_input_listing = false;

        enum class parser_state
        {
            reading_tags,
            reading_gate_list,
            end
        };

        parser_state state = parser_state::reading_tags;

        try
        {
            for (; *s; s = next_line(s))
            {
                linestart = s;
                line++;

                if (state == parser_state::end)
                {
                    break;
                }

                s = opt_ws(s);
                if (is_eol_or_comment(s))
                {
                    continue;
                }

                if (state == parser_state::reading_tags)
                {
                    if (const char* s_begin = opt_insensitive("BEGIN", s))
                    {
                        if (!has_variable_listing)
                            throw std::runtime_error("missing variable listing (.v)");
                        if (!has_input_variable_listing)
                            throw std::runtime_error("missing input variable listing (.i)");
                        if (!has_output_variable_listing)
                            throw std::runtime_error("missing output variable listing (.o)");
                        if (!has_constant_input_listing && spec.num_inputs != spec.num_variables)
                            throw std::runtime_error("missing constant input variable listing (.c)");

                        s = s_begin;
                        state = parser_state::reading_gate_list;
                        continue;
                    }

                    if (const char* s_v = opt_sensitive(".v", s))
                    {
                        if (has_variable_listing)
                            throw std::runtime_error("duplicate variable listing (.v)");

                        has_variable_listing = true;

                        s = s_v;
                        s = opt_ws(s);
                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            if (!std::isalpha(*first))
                            {
                                throw std::runtime_error("variable names must begin with an alpha character");
                            }

                            bool inserted = spec.variable_name_to_id.emplace(std::string(first, last), spec.num_variables).second;
                            if (!inserted)
                            {
                                throw std::runtime_error("duplicate variable name");
                            }

                            spec.variable_names.push_back(std::string(first, last));
                            spec.num_variables += 1;
                        });

                        spec.variable_input_list_index.resize(spec.num_variables, -1);
                        spec.variable_output_list_index.resize(spec.num_variables, -1);
                        spec.variable_constant_input.resize(spec.num_variables, -1);

                        continue;
                    }

                    if (const char* s_i = opt_sensitive(".i", s))
                    {
                        if (!has_variable_listing)
                            throw std::runtime_error("missing variable listing (.v)");
                        if (has_input_variable_listing)
                            throw std::runtime_error("duplicate input variable listing (.i)");

                        has_input_variable_listing = true;

                        s = s_i;
                        s = opt_ws(s);
                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            if (!std::isalpha(*first))
                            {
                                throw std::runtime_error("variable names must begin with an alpha character");
                            }

                            auto it = spec.variable_name_to_id.find(std::string(first, last));
                            if (it == end(spec.variable_name_to_id))
                            {
                                throw std::runtime_error("undeclared input");
                            }

                            if (spec.variable_input_list_index[it->second] != -1)
                            {
                                throw std::runtime_error("duplicate input");
                            }

                            spec.variable_input_list_index[it->second] = spec.num_inputs;

                            spec.input_variable_ids.push_back(it->second);
                            spec.num_inputs += 1;
                        });

                        continue;
                    }

                    if (const char* s_o = opt_sensitive(".o", s))
                    {
                        if (!has_variable_listing)
                            throw std::runtime_error("missing variable listing (.v)");
                        if (has_output_variable_listing)
                            throw std::runtime_error("duplicate output variable listing (.o)");

                        has_output_variable_listing = true;

                        s = s_o;
                        s = opt_ws(s);
                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            if (!std::isalpha(*first))
                            {
                                throw std::runtime_error("variable names must begin with an alpha character");
                            }

                            auto it = spec.variable_name_to_id.find(std::string(first, last));
                            if (it == end(spec.variable_name_to_id))
                            {
                                throw std::runtime_error("undeclared output");
                            }

                            if (spec.variable_output_list_index[it->second] != -1)
                            {
                                throw std::runtime_error("duplicate output");
                            }

                            spec.variable_output_list_index[it->second] = spec.num_outputs;

                            spec.output_variable_ids.push_back(it->second);
                            spec.num_outputs += 1;
                        });

                        continue;
                    }

                    if (const char* s_c = opt_sensitive(".c", s))
                    {
                        if (!has_variable_listing)
                            throw std::runtime_error("missing variable listing (.v)");
                        if (!has_input_variable_listing)
                            throw std::runtime_error("missing input variable listing (.i)");
                        if (has_constant_input_listing)
                            throw std::runtime_error("duplicate constant input variable listing (.c)");

                        has_constant_input_listing = true;

                        int curr_input_var_id = 0;

                        for (;;)
                        {
                            if (curr_input_var_id >= spec.num_variables ||
                                spec.variable_input_list_index[curr_input_var_id] == -1)
                            {
                                break;
                            }

                            curr_input_var_id += 1;
                        }

                        s = s_c;
                        s = opt_ws(s);
                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            int cval = 0;

                            const char* digit = first;
                            while (digit < last && std::isdigit(*digit))
                            {
                                cval = cval * 10;
                                cval += *digit - '0';

                                if (cval > SHRT_MAX)
                                {
                                    throw std::runtime_error("constant value too big");
                                }

                                digit++;
                            }
                            if (digit != last)
                            {
                                throw std::runtime_error("expected number >= 0");
                            }

                            for (;;)
                            {
                                if (curr_input_var_id >= spec.num_variables)
                                {
                                    throw std::runtime_error("more constants than missing inputs");
                                }

                                if (spec.variable_input_list_index[curr_input_var_id] == -1)
                                {
                                    break;
                                }

                                curr_input_var_id += 1;
                            }

                            spec.variable_constant_input[curr_input_var_id] = cval;
                            curr_input_var_id += 1;
                        });

                        if (curr_input_var_id < spec.num_variables)
                        {
                            throw std::runtime_error("not enough constants for non-input variables");
                        }

                        continue;
                    }

                    throw std::runtime_error("expected tag or BEGIN");
                }
                else if (state == parser_state::reading_gate_list)
                {
                    if (const char* s_end = opt_insensitive("END", s))
                    {
                        s = s_end;
                        state = parser_state::end;
                        continue;
                    }

                    bool is_toffoli = std::toupper(*s) == 'T';
                    bool is_fredkin = std::toupper(*s) == 'F';
                    bool is_y = std::toupper(*s) == 'Y';
                    bool is_z = std::toupper(*s) == 'Z';
                    bool is_v = std::toupper(*s) == 'V';
                    bool is_h = std::toupper(*s) == 'H';
                    bool is_q = std::toupper(*s) == 'Q';

                    // single argument gate
                    if (is_toffoli || is_y || is_z || is_v || is_h || is_q)
                    {
                        s++;

                        bool is_notv = false;
                        if (is_v)
                        {
                            if (*s == '\'')
                            {
                                is_notv = true;
                                is_v = false;
                                s++;
                            }
                        }

                        bool is_notq = false;
                        if (is_q)
                        {
                            if (*s == '\'')
                            {
                                is_notq = true;
                                is_q = false;
                                s++;
                            }
                        }

                        int pcnt;
                        s = accept_paramcount(s, &pcnt);
                        s = opt_ws(s);

                        if (pcnt < 1)
                        {
                            throw std::runtime_error("gate needs at least 1 input");
                        }

                        if (is_toffoli)
                            spec.gate_stream.push_back((int)gate_opcode::toffoli);
                        else if (is_y)
                            spec.gate_stream.push_back((int)gate_opcode::pauli_y);
                        else if (is_z)
                            spec.gate_stream.push_back((int)gate_opcode::pauli_z);
                        else if (is_v)
                            spec.gate_stream.push_back((int)gate_opcode::sqrtnot);
                        else if (is_notv)
                            spec.gate_stream.push_back((int)gate_opcode::inv_sqrtnot);
                        else if (is_h)
                            spec.gate_stream.push_back((int)gate_opcode::hadamard);
                        else if (is_q)
                            spec.gate_stream.push_back((int)gate_opcode::rotate_pi_by_4);
                        else if (is_notq)
                            spec.gate_stream.push_back((int)gate_opcode::inv_rotate_pi_by_4);
                        else
                            throw std::logic_error("unhandled single argument gate type");

                        spec.gate_stream.push_back(pcnt);

                        size_t first_param_i = spec.gate_stream.size();

                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            if (pcnt == 0)
                            {
                                throw std::runtime_error("too many parameters");
                            }

                            auto found = spec.variable_name_to_id.find(std::string(first, last));
                            if (found == end(spec.variable_name_to_id))
                            {
                                throw std::runtime_error("undeclared variable");
                            }

                            spec.gate_stream.push_back(found->second);

                            pcnt -= 1;
                        });

                        int last_var = -1;
                        for (size_t i = first_param_i; i < spec.gate_stream.size() - 1; i++)
                        {
                            if (last_var == -1)
                            {
                                last_var = spec.gate_stream[i];
                                continue;
                            }

                            if (last_var >= spec.gate_stream[i])
                            {
                                throw std::runtime_error("parameters must be in variable order");
                            }

                            last_var = spec.gate_stream[i];
                        }

                        continue;
                    }

                    // two argument gate
                    if (is_fredkin)
                    {
                        s++;
                        int pcnt;
                        s = accept_paramcount(s, &pcnt);
                        s = opt_ws(s);

                        if (pcnt < 2)
                        {
                            throw std::runtime_error("gate needs at least 2 inputs");
                        }

                        spec.gate_stream.push_back((int)gate_opcode::fredkin);
                        spec.gate_stream.push_back(pcnt);

                        size_t first_param_i = spec.gate_stream.size();

                        s = accept_list(s, [&](const char* first, const char* last)
                        {
                            if (pcnt == 0)
                            {
                                throw std::runtime_error("too many parameters");
                            }

                            auto found = spec.variable_name_to_id.find(std::string(first, last));
                            if (found == end(spec.variable_name_to_id))
                            {
                                throw std::runtime_error("undeclared variable");
                            }

                            spec.gate_stream.push_back(found->second);

                            pcnt -= 1;
                        });

                        int last_var = -1;
                        for (size_t i = first_param_i; i < spec.gate_stream.size() - 1; i++)
                        {
                            if (last_var == -1)
                            {
                                last_var = spec.gate_stream[i];
                                continue;
                            }

                            if (last_var >= spec.gate_stream[i])
                            {
                                throw std::runtime_error("parameters must be in variable order");
                            }

                            last_var = spec.gate_stream[i];
                        }

                        continue;
                    }

                    throw std::runtime_error("expected gate or END");
                }
                else
                {
                    throw std::logic_error("invalid parser state");
                }
            }
        }
        catch (const std::exception& e)
        {
            throw std::runtime_error(std::to_string(line) + ":" + std::to_string(s - linestart) + ": " + e.what());
        }

        return spec;
    };

    return parse_spec(txt);
}

class qmdd
{
public:
    // to configure p-valued logic
    static const int p = 2;

    struct node_handle
    { 
        uint32_t value;

        bool operator==(const node_handle& other) const
        {
            return value == other.value;
        }

        bool operator!=(const node_handle& other) const
        {
            return !(operator==(other));
        }
    };

    struct weight_handle
    { 
        uint32_t value;

        bool operator==(const weight_handle& other) const
        {
            return value == other.value;
        }

        bool operator!=(const weight_handle& other) const
        {
            return !(operator==(other));
        }
    };

    static constexpr node_handle invalid_node = node_handle{ uint32_t(-1) };
    static constexpr weight_handle invalid_weight = weight_handle{ uint32_t(-1) };

    static constexpr weight_handle weight_0_handle = weight_handle{ 0 };
    static constexpr weight_handle weight_1_handle = weight_handle{ 1 };

    struct edge
    {
        weight_handle w;
        node_handle v;

        edge()
            : w(invalid_weight)
            , v(invalid_node)
        { }

        edge(weight_handle ww, node_handle vv)
            : w(ww), v(vv)
        { }

        explicit edge(node_handle vv)
            : w(weight_1_handle), v(vv)
        { }

        bool operator==(const edge& other) const
        {
            return w == other.w && v == other.v;
        }

        bool operator!=(const edge& other) const
        {
            return !(operator==(other));
        }
    };

    enum edge_op
    {
        edge_op_add,
        edge_op_mul,
        edge_op_kro
    };

    enum weight_op
    {
        weight_op_add,
        weight_op_sub,
        weight_op_mul,
        weight_op_div
    };

private:
    class weight
    {
        // rational algorithms from Boost.Rational (see boost/rational.hpp for explanation)
        class rational
        {
            int num;
            int den;

            static int gcd_euclidean(int a, int b)
            {
                return b == 0 ? a : gcd_euclidean(b, a % b);
            }

            static int gcd(int a, int b)
            {
                int result = gcd_euclidean(a, b);
                return result < 0 ? -result : result;
            }

        public:
            rational() = default;

            explicit rational(int n)
                : num(n), den(1)
            { }

            int numerator() const
            {
                return num;
            }

            int denominator() const
            {
                return den;
            }

            bool operator==(const rational& other) const
            {
                return num == other.num && den == other.den;
            }

            bool operator!=(const rational& other) const
            {
                return !(operator==(other));
            }

            rational& operator+=(const rational& other)
            {
                // Protect against self-modification
                int r_num = other.num;
                int r_den = other.den;

                int g = gcd(den, r_den);
                den /= g;
                num = num * (r_den / g) + r_num * den;
                g = gcd(num, g);
                num /= g;
                den *= r_den / g;
                
                return *this;
            }

            rational& operator-=(const rational& other)
            {
                // Protect against self-modification
                int r_num = other.num;
                int r_den = other.den;

                int g = gcd(den, r_den);
                den /= g;
                num = num * (r_den / g) - r_num * den;
                g = gcd(num, g);
                num /= g;
                den *= r_den / g;

                return *this;
            }

            rational& operator*=(const rational& other)
            {
                // Protect against self-modification
                int r_num = other.num;
                int r_den = other.den;

                int gcd1 = gcd(num, r_den);
                int gcd2 = gcd(r_num, den);
                num = (num / gcd1) * (r_num / gcd2);
                den = (den / gcd2) * (r_den / gcd1);

                return *this;
            }

            rational& operator/=(const rational& other)
            {
                // Protect against self-modification
                int r_num = other.num;
                int r_den = other.den;

                assert(r_num != 0);

                if (num == 0)
                    return *this;

                int gcd1 = gcd(num, r_num);
                int gcd2 = gcd(r_den, den);
                num = (num / gcd1) * (r_den / gcd2);
                den = (den / gcd2) * (r_num / gcd1);

                if (den < 0)
                {
                    num = -num;
                    den = -den;
                }

                return *this;
            }

            rational operator+(const rational& other) const
            {
                rational tmp(*this);
                return tmp += other;
            }

            rational operator-(const rational& other) const
            {
                rational tmp(*this);
                return tmp -= other;
            }

            rational operator*(const rational& other) const
            {
                rational tmp(*this);
                return tmp *= other;
            }

            rational operator/(const rational& other) const
            {
                rational tmp(*this);
                return tmp /= other;
            }
        };

        class irrational
        {
            rational intpart;
            rational sqrt2part;

        public:
            irrational() = default;

            explicit irrational(const rational& i)
                : intpart(i)
                , sqrt2part(0)
            { }

            irrational(const rational& i, const rational& sq2)
                : intpart(i)
                , sqrt2part(sq2)
            { }

            rational integer() const
            {
                return intpart;
            }

            rational sqrt2() const
            {
                return sqrt2part;
            }

            bool operator==(const irrational& other) const
            {
                return intpart == other.intpart && sqrt2part == other.sqrt2part;
            }

            irrational& operator+=(const irrational& other)
            {
                intpart += other.intpart;
                sqrt2part += other.sqrt2part;

                return *this;
            }

            irrational& operator-=(const irrational& other)
            {
                intpart -= other.intpart;
                sqrt2part -= other.sqrt2part;

                return *this;
            }

            irrational& operator*=(const irrational& other)
            {
                // (a + b sqrt(2)) * (c + d sqrt(2))
                // = ac + ad sqrt(2) + bc sqrt(2) + bd 2
                // = (ac + 2bd) + (ad + bc) sqrt(2)
                rational a = intpart, b = sqrt2part, c = other.intpart, d = other.sqrt2part;
                
                intpart = a * c + rational(2) * b * d;
                sqrt2part = a * d + b * c;

                return *this;
            }

            irrational& operator/=(const irrational& other)
            {
                // derived with wolfram alpha lol
                rational a = intpart, b = sqrt2part, c = other.intpart, d = other.sqrt2part;

                rational denom = (c * c - rational(2) * d * d);
                intpart = (a * c - rational(2) * b * d) / denom;
                sqrt2part = (b * c - a * d) / denom;

                return *this;
            }

            irrational operator+(const irrational& other) const
            {
                irrational tmp(*this);
                return tmp += other;
            }

            irrational operator-(const irrational& other) const
            {
                irrational tmp(*this);
                return tmp -= other;
            }

            irrational operator*(const irrational& other) const
            {
                irrational tmp(*this);
                return tmp *= other;
            }

            irrational operator/(const irrational& other) const
            {
                irrational tmp(*this);
                return tmp /= other;
            }
        };

        irrational real;
        irrational imag;

    public:
        weight() = default;

        static weight zero()
        {
            weight w;
            w.real = irrational(rational(0));
            w.imag = irrational(rational(0));
            return w;
        }

        static weight one()
        {
            weight w;
            w.real = irrational(rational(1));
            w.imag = irrational(rational(0));
            return w;
        }

        static weight i()
        {
            weight w;
            w.real = irrational(rational(0));
            w.imag = irrational(rational(1));
            return w;
        }

        static weight sq2()
        {
            weight w;
            w.real = irrational(rational(0), rational(1));
            w.imag = irrational(rational(0));
            return w;
        }

        bool operator==(const weight& other) const
        {
            return real == other.real && imag == other.imag;
        }

        bool operator!=(const weight& other) const
        {
            return !(operator==(other));
        }

        weight& operator+=(const weight& other)
        {
            // (a + bi) + (c + di)
            // = (a + c) + (b + d)i
            irrational a = real, b = imag, c = other.real, d = other.imag;

            real = a + c;
            imag = b + d;

            return *this;
        }

        weight operator+(const weight& other) const
        {
            weight tmp(*this);
            return tmp += other;
        }

        weight& operator-=(const weight& other)
        {
            // (a + bi) - (c + di)
            // = (a - c) + (b - d)i
            irrational a = real, b = imag, c = other.real, d = other.imag;

            real = a - c;
            imag = b - d;

            return *this;
        }

        weight operator-(const weight& other) const
        {
            weight tmp(*this);
            return tmp -= other;
        }

        weight& operator*=(const weight& other)
        {
            // (a + bi) * (c + di)
            // = ac + adi + bci - bd
            // = (ac - bd) + (ad + bc)i
            irrational a = real, b = imag, c = other.real, d = other.imag;
            
            real = a * c - b * d;
            imag = a * d + b * c;

            return *this;
        }

        weight operator*(const weight& other) const
        {
            weight tmp(*this);
            return tmp *= other;
        }

        weight& operator/=(const weight& other)
        {
            // proof: http://mathworld.wolfram.com/ComplexDivision.html
            irrational a = real, b = imag, c = other.real, d = other.imag;

            irrational denom = c * c + d * d;
            real = (a * c + b * d) / denom;
            imag = (b * c - a * d) / denom;

            return *this;
        }

        weight operator/(const weight& other) const
        {
            weight tmp(*this);
            return tmp /= other;
        }

        std::string to_string() const
        {
            static const auto rational_to_string = [](const rational& r, char mode)
            {
                std::string s;

                if (mode == 'r')
                {
                    s += std::to_string(r.numerator());
                }
                
                if (mode == 'i')
                {
                    if (r.numerator() == 1)
                    {
                        s += "i";
                    }
                    else if (r.numerator() == -1)
                    {
                        s += "-i";
                    }
                    else
                    {
                        s += std::to_string(r.numerator()) + "i";
                    }
                }

                if (r.denominator() != 1)
                {
                    s += "/" + std::to_string(r.denominator());
                }

                return s;
            };

            static const auto irrational_to_string = [](const irrational& ir, char mode)
            {
                std::string s;

                bool wrote_intpart = false;
                if (ir.integer() != rational(0))
                {
                    s += rational_to_string(ir.integer(), mode);

                    wrote_intpart = true;
                }
                
                bool wrote_sqrt2part = false;
                if (ir.sqrt2() != rational(0))
                {
                    if (wrote_intpart && ir.sqrt2().numerator() > 0)
                    {
                        s += "+";
                    }

                    if (ir.sqrt2().denominator() != 1)
                        s += "(";

                    s += rational_to_string(ir.sqrt2(), mode);

                    if (ir.sqrt2().denominator() != 1)
                        s += ")";

                    // utf8 sqrt symbol
                    s += "\xe2\x88\x9a";

                    s += "2";

                    wrote_sqrt2part = true;
                }

                if (!wrote_intpart && !wrote_sqrt2part)
                {
                    s += "0";
                }

                return s;
            };

            static const auto complex_to_string = [](const irrational& real, const irrational& imag)
            {
                std::string s;

                int num_real_terms = int(real.integer() != rational(0)) + int(real.sqrt2() != rational(0));
                if (num_real_terms > 0)
                {
                    if (num_real_terms > 1)
                        s += "(";

                    s += irrational_to_string(real, 'r');

                    if (num_real_terms > 1)
                        s += ")";
                }

                int num_imag_terms = int(imag.integer() != rational(0)) + int(imag.sqrt2() != rational(0));
                if (num_imag_terms > 0)
                {
                    if (num_real_terms > 0)
                        s += "+";

                    if (num_imag_terms > 1)
                        s += "(";

                    s += irrational_to_string(imag, 'i');

                    if (num_imag_terms > 1)
                        s += ")";
                }

                if (num_real_terms == 0 && num_imag_terms == 0)
                {
                    s += "0";
                }

                return s;
            };

            std::string s = complex_to_string(real, imag);

            return s;
        }
    };

    class unique_table
    {
        struct node
        {
            uint32_t var;
            std::array<node_handle,p*p> children;
            std::array<weight_handle, p*p> weights;

            bool operator==(const node& other) const
            {
                return std::tie(var, children, weights) == std::tie(other.var, other.children, other.weights);
            }
        };

        static const uint32_t capacity = 0x100000;
        static_assert((capacity & (capacity - 1)) == 0, "capacity must be power of two");

        static const uint32_t ddutmask = capacity - 1;

        std::unique_ptr<node[]> node_pool;

        uint32_t pool_head;

        node* pool_alloc()
        {
            uint32_t old_head = pool_head++;

            if (old_head >= capacity)
            {
                printf("pool_alloc failed\n");
                std::abort();
            }

            return &node_pool[old_head];
        }

        std::unique_ptr<node_handle[]> table;

        node* true_node;

        node_handle to_handle(const node* n) const
        {
            return node_handle{ uint32_t(n - &node_pool[0]) };
        }

        const node* to_node(node_handle h) const
        {
            return (const node*)&node_pool[h.value];
        }

    public:
        void init(uint32_t num_vars)
        {
            node_pool.reset(new node[capacity]);
            pool_head = 0;

            table.reset(new node_handle[capacity]);
            for (uint32_t i = 0; i < capacity; i++)
            {
                table[i] = invalid_node;
            }

            true_node = pool_alloc();
            true_node->var = num_vars;
            for (int i = 0; i < p*p; i++)
            {
                true_node->children[i] = to_handle(true_node);
                true_node->weights[i] = weight_1_handle;
            }
        }

        node_handle get_true() const
        {
            return to_handle(true_node);
        }

        int get_var(node_handle h) const
        {
            return to_node(h)->var;
        }

        void get_children(node_handle h, node_handle children[p*p]) const
        {
            const node* n = to_node(h);
            for (int i = 0; i < p*p; i++)
            {
                children[i] = n->children[i];
            }
        }

        node_handle get_child(node_handle h, int i) const
        {
            return to_node(h)->children[i];
        }

        void get_weights(node_handle h, weight_handle weights[p*p]) const
        {
            const node* n = to_node(h);
            for (int i = 0; i < p*p; i++)
            {
                weights[i] = n->weights[i];
            }
        }

        weight_handle get_weight(node_handle h, int i) const
        {
            return to_node(h)->weights[i];
        }

        node_handle insert(uint32_t var, const node_handle children[p*p], const weight_handle weights[p*p])
        {
            node n;
            n.var = var;
            for (int i = 0; i < p*p; i++)
            {
                n.children[i] = children[i];
                n.weights[i] = weights[i];
            }

            uint32_t key = var;
            for (int i = 0; i < p*p; i++)
            {
                key += children[i].value + weights[i].value;
            }
            key = key & ddutmask;

            while (table[key] != invalid_node)
            {
                const node* curr = to_node(table[key]);
                if (*curr == n)
                {
                    return to_handle(curr);
                }
                key = (key + 1) & ddutmask;
            }

            node* new_node = pool_alloc();
            *new_node = n;

            node_handle handle = to_handle(new_node);
            table[key] = handle;
            return handle;
        }
    };

    class computed_table
    {
        struct cache_entry
        {
            edge e0;
            edge e1;
            edge_op op;
            edge result;
        };

        static const uint32_t ctsize = 1024;
        static_assert((ctsize & (ctsize - 1)) == 0, "ctsize must be a power of two");

        static const uint32_t ctmask = ctsize - 1;

        std::vector<cache_entry> cache;

        static uint32_t hash(const edge& e0, const edge& e1, edge_op op)
        {
            return (e0.v.value + e1.v.value + e0.w.value + e1.w.value + op) & ctmask;
        }

    public:
        computed_table()
            : cache(ctsize, cache_entry{ edge(), edge(), (edge_op)0, edge() })
        { }

        edge find(const edge& e0, const edge& e1, edge_op op) const
        {
            uint32_t key = hash(e0, e1, op);

            const cache_entry& entry = cache[key];
            if (entry.e0 == e0 && entry.e1 == e1 && entry.op == op)
                return entry.result;
            else
                return edge(invalid_weight, invalid_node);
        }

        void insert(const edge& e0, const edge& e1, edge_op op, const edge& r)
        {
            uint32_t key = hash(e0, e1, op);
            cache[key] = cache_entry{ e0, e1, op, r };
        }
    };

    class unique_weights
    {
        std::vector<weight> weights;

    public:
        unique_weights()
        {
            weights.push_back(weight::zero());
            weights.push_back(weight::one());
        }

        weight_handle insert(const weight& w)
        {
            for (size_t i = 0; i < weights.size(); i++)
            {
                if (weights[i] == w)
                {
                    return weight_handle{ uint32_t(i) };
                }
            }

            weights.push_back(w);
            return weight_handle{ uint32_t(weights.size() - 1) };
        }

        weight get_weight(weight_handle w) const
        {
            return weights[w.value];
        }
    };

    class computed_weights
    {
        struct cache_entry
        {
            weight_handle w0;
            weight_handle w1;
            weight_op op;
            weight_handle result;
        };

        static const uint32_t ctsize = 1024;
        static_assert((ctsize & (ctsize - 1)) == 0, "ctsize must be a power of two");

        static const uint32_t ctmask = ctsize - 1;

        std::vector<cache_entry> cache;

        static uint32_t hash(weight_handle w0, weight_handle w1, weight_op op)
        {
            return (w0.value + w1.value + op) & ctmask;
        }

    public:
        computed_weights()
            : cache(ctsize, cache_entry{ invalid_weight, invalid_weight, (weight_op)0, invalid_weight })
        { }

        weight_handle find(weight_handle w0, weight_handle w1, weight_op op) const
        {
            uint32_t key = hash(w0, w1, op);

            const cache_entry& entry = cache[key];
            if (entry.w0 == w0 && entry.w1 == w1 && entry.op == op)
                return entry.result;
            else
                return invalid_weight;
        }

        void insert(weight_handle w0, weight_handle w1, weight_op op, weight_handle r)
        {
            uint32_t key = hash(w0, w1, op);
            cache[key] = cache_entry{ w0, w1, op, r };
        }
    };

    unique_table uniquetb;
    computed_table computedtb;

    unique_weights uniquewt;
    computed_weights computedwt;

    node_handle true_node;

public:
    explicit qmdd(uint32_t num_vars)
    {
        uniquetb.init(num_vars);

        true_node = uniquetb.get_true();
    }

    node_handle get_true() const
    {
        return true_node;
    }

    int get_var(node_handle h) const
    {
        return uniquetb.get_var(h);
    }

    void get_children(node_handle h, node_handle children[4]) const
    {
        return uniquetb.get_children(h, children);
    }

    void get_weights(node_handle h, weight_handle weights[4]) const
    {
        return uniquetb.get_weights(h, weights);
    }

    node_handle make_node(uint32_t var, const node_handle children[4], const weight_handle weights[4])
    {
        // enforce no-redundancy constraint of QMDD
        bool redundant = true;
        for (int i = 0; i < p*p - 1; i++)
        {
            if (children[i] != children[i + 1] ||
                weights[i] != weights[i + 1])
            {
                redundant = false;
                break;
            }
        }
        if (redundant)
        {
            return children[0];
        }

        // enforce uniqueness constraint of QMDD
        return uniquetb.insert(var, children, weights);
    }

    weight_handle get_weight_i_handle()
    {
        weight weight_i = weight::i();
        return uniquewt.insert(weight_i);
    }

    weight_handle get_weight_sq2_handle()
    {
        weight weight_sq2 = weight::sq2();
        return uniquewt.insert(weight_sq2);
    }

    weight_handle apply(weight_handle w0, weight_handle w1, weight_op op)
    {
        weight_handle found = computedwt.find(w0, w1, op);
        if (found != invalid_weight)
        {
            return found;
        }

        weight new_weight;
        if (op == weight_op_add)
        {
            new_weight = uniquewt.get_weight(w0) + uniquewt.get_weight(w1);
        }
        else if (op == weight_op_sub)
        {
            new_weight = uniquewt.get_weight(w0) - uniquewt.get_weight(w1);
        }
        else if (op == weight_op_mul)
        {
            new_weight = uniquewt.get_weight(w0) * uniquewt.get_weight(w1);
        }
        else if (op == weight_op_div)
        {
            new_weight = uniquewt.get_weight(w0) / uniquewt.get_weight(w1);
        }
        else
        {
            assert(!"unimplemented op");
            return invalid_weight;
        }

        weight_handle w = uniquewt.insert(new_weight);

        computedwt.insert(w0, w1, op, w);

        return w;
    }

    edge apply(const edge& e0, const edge& e1, edge_op op)
    {
        // helper to make the code better match the pseudo-code
        struct apply_helper
        {
            qmdd* const dd;

            weight_handle w(const edge& e)
            {
                return e.w; 
            }

            node_handle v(const edge& e)
            {
                return e.v;
            }

            int x(const edge& e)
            { 
                return dd->get_var(e.v); 
            }

            edge Ei(const edge& e, int i)
            {
                return edge(dd->uniquetb.get_weight(e.v, i), dd->uniquetb.get_child(e.v, i));
            }

            bool term(const edge& e)
            { 
                return e.v == dd->get_true();
            }

            weight_handle normalize(weight_handle children[p*p])
            {
                for (int i = 0; i < p*p; i++)
                {
                    if (children[i] != weight_0_handle)
                    {
                        weight_handle edge_weight = children[i];

                        // normalize this child
                        children[i] = weight_1_handle;

                        // normalize the other children
                        for (int j = i + 1; j < p*p; j++)
                        {
                            if (children[j] != weight_0_handle)
                            {
                                children[j] = dd->apply(children[j], edge_weight, weight_op_div);
                            }
                        }

                        return edge_weight;
                    }
                }

                // all weights were 0
                return weight_0_handle;
            }

            edge add(const edge& e0, const edge& e1)
            {
                if (term(e0))
                {
                    // addition with 0 always returns other input
                    if (w(e0) == weight_0_handle)
                        return e1;
                    else if (term(e1))
                        return edge(dd->apply(w(e0), w(e1), weight_op_add), dd->get_true());
                }

                // enforce variable order of e0 < e1
                if (x(e0) > x(e1))
                    return add(e1, e0);

                // recursively add the p*p quadrants
                node_handle z_children[p*p];
                weight_handle z_weights[p*p];
                for (int i = 0; i < p*p; i++)
                {
                    edge q0 = edge(dd->apply(w(e0), w(Ei(e0, i)), weight_op_mul), v(Ei(e0, i)));

                    edge q1;
                    if (x(e0) == x(e1))
                        q1 = edge(dd->apply(w(e1), w(Ei(e1, i)), weight_op_mul), v(Ei(e1, i)));
                    else
                        q1 = e1;

                    edge zi = dd->apply(q0, q1, edge_op_add);
                    z_children[i] = zi.v;
                    z_weights[i] = zi.w;
                }

                // normalize weights
                weight_handle new_weight = normalize(z_weights);

                // make the new node
                node_handle new_node = dd->make_node(x(e0), z_children, z_weights);

                return edge(new_weight, new_node);
            }

            edge mul(const edge& e0, const edge& e1)
            {
                // multiplication with "1" terminal as base case
                if (term(e0))
                {
                    // multiplication with 0 always returns 0
                    if (w(e0) == weight_0_handle)
                        return e0;
                    else if (w(e0) == weight_1_handle)
                        return e1; // avoid multiplication for the case of multiplying by 1
                    else
                        return edge(dd->apply(w(e0), w(e1), weight_op_mul), v(e1));
                }

                // enforce variable order of e0 < e1
                if (x(e0) > x(e1))
                    return add(e1, e0);

                // recursively multiply the p*p quadrants
                node_handle z_children[p*p];
                weight_handle z_weights[p*p];
                for (int i = 0; i < p * p; i += p)
                {
                    for (int j = 0; j < p; j++)
                    {
                        edge zij = edge(weight_0_handle, dd->get_true());
                        for (int k = 0; k < p; k++)
                        {
                            edge q0 = edge(dd->apply(w(e0), w(Ei(e0, i + k)), weight_op_mul), v(Ei(e0, i + k)));

                            edge q1;
                            if (x(e0) == x(e1))
                                q1 = edge(dd->apply(w(e1), w(Ei(e1, j + p*k)), weight_op_mul), v(Ei(e1, j + p*k)));
                            else
                                q1 = e1;

                            edge q0q1 = dd->apply(q0, q1, edge_op_mul);
                            zij = dd->apply(zij, q0q1, edge_op_add);
                        }
                        z_children[i + j] = zij.v;
                        z_weights[i + j] = zij.w;
                    }
                }

                // normalize weights
                weight_handle new_weight = normalize(z_weights);

                node_handle new_node = dd->make_node(x(e0), z_children, z_weights);

                return edge(new_weight, new_node);
            }

            edge kro(const edge& e0, const edge& e1)
            {
                // kronecker with the "1" terminal as a base case
                if (term(e0))
                {
                    // kronecker with 0 always gives 0
                    if (w(e0) == weight_0_handle)
                        return e0;
                    else if (w(e0) == weight_1_handle)
                        return e1; // avoid multiplication for the case of multiplying by 1
                    else
                        return edge(dd->apply(w(e0), w(e1), weight_op_mul), v(e1));
                }
                
                // simplifying assumption to avoid having to determine which variable is the top one
                assert(x(e0) < x(e1));

                // recursively kronecker
                node_handle z_children[p*p];
                weight_handle z_weights[p*p];
                for (int i = 0; i < p*p; i++)
                {
                    edge zi = dd->apply(Ei(e0, i), e1, edge_op_kro);
                    z_children[i] = zi.v;
                    z_weights[i] = zi.w;
                }

                // normalize weights
                weight_handle new_weight = normalize(z_weights);
                new_weight = dd->apply(new_weight, w(e0), weight_op_mul);

                // make the new node
                node_handle new_node = dd->make_node(x(e0), z_children, z_weights);

                return edge(new_weight, new_node);
            }
        };

        edge found = computedtb.find(e0, e1, op);
        if (found.v != invalid_node)
        {
            return found;
        }

        apply_helper helper{ this };

        edge new_edge;

        if (op == edge_op_add)
        {
            new_edge = helper.add(e0, e1);
        }
        else if (op == edge_op_mul)
        {
            new_edge = helper.mul(e0, e1);
        }
        else if (op == edge_op_kro)
        {
            new_edge = helper.kro(e0, e1);
        }
        else
        {
            assert(!"unimplemented op");
            return edge(invalid_weight, invalid_node);
        }

        computedtb.insert(e0, e1, op, new_edge);

        return new_edge;
    }

    std::string to_string(weight_handle w) const
    {
        return uniquewt.get_weight(w).to_string();
    }
};

qmdd decode(const program_spec& spec, qmdd::edge* root_out)
{
    using node_handle = qmdd::node_handle;
    using weight_handle = qmdd::weight_handle;
    using edge = qmdd::edge;

    static const int p = qmdd::p;
    static const weight_handle weight_0_handle = qmdd::weight_0_handle;
    static const weight_handle weight_1_handle = qmdd::weight_1_handle;

    qmdd dd(spec.num_variables);

    node_handle true_node = dd.get_true();

    node_handle identity_children[p * p];
    weight_handle identity_weights[p * p];
    weight_handle not_weights[p * p];
    for (int i = 0; i < p; i++)
    {
        for (int j = 0; j < p; j++)
        {
            identity_children[i * p + j] = true_node;

            if (i == j)
            {
                identity_weights[i * p + j] = weight_1_handle;
                not_weights[i * p + j] = weight_0_handle;
            }
            else
            {
                identity_weights[i * p + j] = weight_0_handle;
                not_weights[i * p + j] = weight_1_handle;
            }
        }
    }

    weight_handle zero_weights[p * p];
    for (int i = 0; i < p * p; i++)
    {
        zero_weights[i] = weight_0_handle;
    }

    weight_handle if_false_weights[p * p];
    for (int i = 0; i < p * p; i++)
    {
        if (i == 0)
            if_false_weights[i] = weight_1_handle;
        else
            if_false_weights[i] = weight_0_handle;
    }

    weight_handle if_true_weights[p * p];
    for (int i = 0; i < p * p; i++)
    {
        if (i == p * p - 1)
            if_true_weights[i] = weight_1_handle;
        else
            if_true_weights[i] = weight_0_handle;
    }

    weight_handle y_weights[p * p];
    if (p == 2)
    {
        y_weights[0] = weight_0_handle;
        y_weights[1] = dd.apply(weight_0_handle, dd.get_weight_i_handle(), qmdd::weight_op_sub);
        y_weights[2] = dd.get_weight_i_handle();
        y_weights[3] = weight_0_handle;
    }

    weight_handle z_weights[p * p];
    if (p == 2)
    {
        z_weights[0] = weight_1_handle;
        z_weights[1] = weight_0_handle;
        z_weights[2] = weight_0_handle;
        z_weights[3] = dd.apply(weight_0_handle, weight_1_handle, qmdd::weight_op_sub);
    }

    weight_handle sqrtnot_weights[p * p];
    weight_handle inv_sqrtnot_weights[p * p];
    if (p == 2)
    {
        weight_handle weight_2_handle = dd.apply(weight_1_handle, weight_1_handle, qmdd::weight_op_add);

        weight_handle one_add_i_by_2 = dd.apply(dd.apply(weight_1_handle, dd.get_weight_i_handle(), qmdd::weight_op_add), weight_2_handle, qmdd::weight_op_div);
        weight_handle one_sub_i_by_2 = dd.apply(dd.apply(weight_1_handle, dd.get_weight_i_handle(), qmdd::weight_op_sub), weight_2_handle, qmdd::weight_op_div);
        
        sqrtnot_weights[0] = one_add_i_by_2;
        sqrtnot_weights[1] = one_sub_i_by_2;
        sqrtnot_weights[2] = one_sub_i_by_2;
        sqrtnot_weights[3] = one_add_i_by_2;

        inv_sqrtnot_weights[0] = one_sub_i_by_2;
        inv_sqrtnot_weights[1] = one_add_i_by_2;
        inv_sqrtnot_weights[2] = one_add_i_by_2;
        inv_sqrtnot_weights[3] = one_sub_i_by_2;
    }

    weight_handle hadamard_weights[p * p];
    if (p == 2)
    {
        weight_handle one_by_sq2 = dd.apply(weight_1_handle, dd.get_weight_sq2_handle(), qmdd::weight_op_div);

        hadamard_weights[0] = one_by_sq2;
        hadamard_weights[1] = one_by_sq2;
        hadamard_weights[2] = one_by_sq2;
        hadamard_weights[3] = dd.apply(weight_0_handle, one_by_sq2, qmdd::weight_op_sub);
    }

    weight_handle rotate_pi_by_4_weights[p * p];
    weight_handle inv_rotate_pi_by_4_weights[p * p];
    if (p == 2)
    {
        weight_handle one_by_sq2 = dd.apply(weight_1_handle, dd.get_weight_sq2_handle(), qmdd::weight_op_div);
        weight_handle i_by_sq2 = dd.apply(dd.get_weight_i_handle(), dd.get_weight_sq2_handle(), qmdd::weight_op_div);

        rotate_pi_by_4_weights[0] = weight_1_handle;
        rotate_pi_by_4_weights[1] = weight_0_handle;
        rotate_pi_by_4_weights[2] = weight_0_handle;
        rotate_pi_by_4_weights[3] = dd.apply(one_by_sq2, i_by_sq2, qmdd::weight_op_add);

        inv_rotate_pi_by_4_weights[0] = weight_1_handle;
        inv_rotate_pi_by_4_weights[1] = weight_0_handle;
        inv_rotate_pi_by_4_weights[2] = weight_0_handle;
        inv_rotate_pi_by_4_weights[3] = dd.apply(one_by_sq2, i_by_sq2, qmdd::weight_op_sub);
    }

    edge root = edge(weight_1_handle, true_node);

    // initialize circuit with p^n by p^n identity
    std::vector<edge> identitySubtree(spec.num_variables + 1);
    identitySubtree[spec.num_variables] = root;
    for (int var_id = spec.num_variables - 1; var_id >= 0; var_id--)
    {
        root = dd.apply(edge(dd.make_node(var_id, identity_children, identity_weights)), root, qmdd::edge_op_kro);
        identitySubtree[var_id] = root;
    }

    struct gate_stream_view
    {
        const int* stream;
        int size;
        int offset;
    };

    // stack of gate streams to execute.
    // this is useful to implement "micro-coded" gates.
    std::vector<gate_stream_view> gate_streams;
    gate_streams.push_back(gate_stream_view{ spec.gate_stream.data(), (int)spec.gate_stream.size(), 0 });

    // storage for microcode
    std::vector<int> fredkin_microcode;

    // prevents updates to gate_streams from invalidating pointers...
    auto curr_stream = [&gate_streams]() -> gate_stream_view& { return gate_streams.back(); };

    while (!gate_streams.empty())
    {
        if (curr_stream().offset == curr_stream().size)
        {
            gate_streams.pop_back();
            continue;
        }

        gate_opcode opcode = (gate_opcode)curr_stream().stream[curr_stream().offset];
        curr_stream().offset++;

        int param_count = curr_stream().stream[curr_stream().offset];
        curr_stream().offset++;
        
        const int* first_param = &curr_stream().stream[curr_stream().offset];
        const int* last_param = &curr_stream().stream[curr_stream().offset] + param_count;

        curr_stream().offset += param_count;

        switch (opcode)
        {
        case gate_opcode::toffoli:
        case gate_opcode::pauli_y:
        case gate_opcode::pauli_z:
        case gate_opcode::sqrtnot:
        case gate_opcode::inv_sqrtnot:
        case gate_opcode::hadamard:
        case gate_opcode::rotate_pi_by_4:
        case gate_opcode::inv_rotate_pi_by_4:
        {
#ifdef SHOW_INSTRS
            printf("%s%d ", 
                opcode == gate_opcode::toffoli ? "t" :
                opcode == gate_opcode::pauli_y ? "y" :
                opcode == gate_opcode::pauli_z ? "z" :
                opcode == gate_opcode::sqrtnot ? "v" :
                opcode == gate_opcode::inv_sqrtnot ? "v\'" :
                opcode == gate_opcode::hadamard ? "h" :
                opcode == gate_opcode::rotate_pi_by_4 ? "q" :
                opcode == gate_opcode::inv_rotate_pi_by_4 ? "q\'" :
                "?",
                param_count);
            for (const int* param = first_param; param < last_param; param++)
            {
                if (param != first_param)
                {
                    printf(",");
                }
                printf("%s", spec.variable_names[*param].c_str());
            }
            printf("\n");
#endif
            const weight_handle* gate_weights;
            if (opcode == gate_opcode::toffoli)
            {
                if (p != 2)
                {
                    assert(!"not gates not allowed outside of 2-valued logic");
                }
                gate_weights = not_weights;
            }
            else if (opcode == gate_opcode::pauli_y)
            {
                if (p != 2)
                {
                    assert(!"y gates not allowed outside of 2-valued logic");
                }
                gate_weights = y_weights;
            }
            else if (opcode == gate_opcode::pauli_z)
            {
                if (p != 2)
                {
                    assert(!"z gates not allowed outside of 2-valued logic");
                }
                gate_weights = z_weights;
            }
            else if (opcode == gate_opcode::sqrtnot)
            {
                if (p != 2)
                {
                    assert(!"v gates not allowed outside of 2-valued logic");
                }
                gate_weights = sqrtnot_weights;
            }
            else if (opcode == gate_opcode::inv_sqrtnot)
            {
                if (p != 2)
                {
                    assert(!"v\' gates not allowed outside of 2-valued logic");
                }
                gate_weights = inv_sqrtnot_weights;
            }
            else if (opcode == gate_opcode::hadamard)
            {
                if (p != 2)
                {
                    assert(!"hadamard gates not allowed outside of 2-valued logic");
                }
                gate_weights = hadamard_weights;
            }
            else if (opcode == gate_opcode::rotate_pi_by_4)
            {
                if (p != 2)
                {
                    assert(!"q gates not allowed outside of 2-valued logic");
                }
                gate_weights = rotate_pi_by_4_weights;
            }
            else if (opcode == gate_opcode::inv_rotate_pi_by_4)
            {
                if (p != 2)
                {
                    assert(!"q\' gates not allowed outside of 2-valued logic");
                }
                gate_weights = inv_rotate_pi_by_4_weights;
            }
            else
            {
                assert(!"unhandled gate type");
            }

            assert(last_param - first_param >= 1);

            int target_var_id = *(last_param - 1);
            const int* next_control_var_id = last_param - 2;

            edge active_gate = edge(weight_1_handle, true_node);
            edge inactive_gate = edge(weight_0_handle, true_node);

            for (int var_id = spec.num_variables - 1; var_id >= 0; var_id--)
            {
                bool is_control = false;
                if (next_control_var_id >= first_param && *next_control_var_id == var_id)
                {
                    is_control = true;
                    next_control_var_id -= 1;
                }

                if (var_id > target_var_id) // variables below the target
                {
                    if (is_control)
                    {
                        active_gate = dd.apply(edge(dd.make_node(var_id, identity_children, if_true_weights)), active_gate, qmdd::edge_op_kro);

                        inactive_gate = dd.apply(
                            dd.apply(edge(dd.make_node(var_id, identity_children, if_false_weights)), identitySubtree[var_id + 1], qmdd::edge_op_kro),
                            dd.apply(edge(dd.make_node(var_id, identity_children, if_true_weights)), inactive_gate, qmdd::edge_op_kro),
                            qmdd::edge_op_add);
                    }
                    else
                    {
                        active_gate = dd.apply(edge(dd.make_node(var_id, identity_children, identity_weights)), active_gate, qmdd::edge_op_kro);
                        inactive_gate = dd.apply(edge(dd.make_node(var_id, identity_children, identity_weights)), inactive_gate, qmdd::edge_op_kro);
                    }
                }
                else if (var_id == target_var_id) // the target variable
                {
                    active_gate = dd.apply(
                        dd.apply(edge(dd.make_node(var_id, identity_children, identity_weights)), inactive_gate, qmdd::edge_op_kro),
                        dd.apply(edge(dd.make_node(var_id, identity_children, gate_weights)), active_gate, qmdd::edge_op_kro),
                        qmdd::edge_op_add);
                }
                else if (var_id < target_var_id) // variables above the target
                {
                    if (is_control)
                    {
                        active_gate = dd.apply(
                            dd.apply(edge(dd.make_node(var_id, identity_children, if_false_weights)), identitySubtree[var_id + 1], qmdd::edge_op_kro),
                            dd.apply(edge(dd.make_node(var_id, identity_children, if_true_weights)), active_gate, qmdd::edge_op_kro),
                            qmdd::edge_op_add);
                    }
                    else
                    {
                        active_gate = dd.apply(edge(dd.make_node(var_id, identity_children, identity_weights)), active_gate, qmdd::edge_op_kro);
                    }
                }
            }

            root = dd.apply(active_gate, root, qmdd::edge_op_mul);

            break;
        }
        case gate_opcode::fredkin:
        {
#ifdef SHOW_INSTRS
            printf("f%d ", param_count);
            for (const int* param = first_param; param < last_param; param++)
            {
                if (param != first_param)
                {
                    printf(",");
                }
                printf("%s", spec.variable_names[*param].c_str());
            }
            printf("\n");
#endif

            assert(last_param - first_param >= 2);

            int swap_b_var_id = *(last_param - 1);
            int swap_a_var_id = *(last_param - 2);
            int num_controls = (int)(last_param - first_param) - 2;

            fredkin_microcode.clear();

            fredkin_microcode.push_back((int)gate_opcode::toffoli);
            fredkin_microcode.push_back(2);
            fredkin_microcode.push_back(swap_b_var_id);
            fredkin_microcode.push_back(swap_a_var_id);

            fredkin_microcode.push_back((int)gate_opcode::toffoli);
            fredkin_microcode.push_back(2 + num_controls);
            for (int control_idx = 0; control_idx < num_controls; control_idx++)
            {
                fredkin_microcode.push_back(first_param[control_idx]);
            }
            fredkin_microcode.push_back(swap_a_var_id);
            fredkin_microcode.push_back(swap_b_var_id);

            fredkin_microcode.push_back((int)gate_opcode::toffoli);
            fredkin_microcode.push_back(2);
            fredkin_microcode.push_back(swap_b_var_id);
            fredkin_microcode.push_back(swap_a_var_id);

            gate_streams.push_back(gate_stream_view{ fredkin_microcode.data(), (int)fredkin_microcode.size(), 0 });

            break;
        }
        default:
            throw std::logic_error("unknown gate opcode");
        }
    }

    if (root_out) *root_out = root;

    return dd;
}

void write_dot(
    const char* title,
    const program_spec& spec, const qmdd& dd,
    const qmdd::edge& root,
    const char* fn)
{
    static const int p = qmdd::p;

    FILE* f = fopen(fn, "w");

    if (!f)
    {
        throw std::runtime_error(std::string("failed to open ") + fn);
    }

    fprintf(f, "digraph {\n");

    fprintf(f, "  labelloc=\"t\";\n");
    fprintf(f, "  label=\"%s\";\n", title);
    fprintf(f, "  splines=line;\n");

    std::vector<qmdd::node_handle> nodes2add = { root.v };

    qmdd::node_handle true_node = dd.get_true();

    struct node_hasher
    {
        auto operator()(qmdd::node_handle n) const
        {
            return n.value;
        }
    };

    std::unordered_set<qmdd::node_handle, node_hasher> added;
    added.insert(true_node);

    std::unordered_set<qmdd::node_handle, node_hasher> declared;
    
    fprintf(f, "  root [shape=point,width=0.001,height=0.001];\n");
    fprintf(f, "  root -> n%u [label=\"%s\"];\n", root.v.value, dd.to_string(root.w).c_str());

    if (root.v == true_node)
    {
        fprintf(f, "  n%u [label=\"1\",shape=box];\n", true_node.value);
    }
    else
    {
        fprintf(f, "  n%u [label=\"%s\",shape=circle];\n", root.v.value, spec.variable_names[dd.get_var(root.v)].c_str());
    }

    declared.insert(root.v);

    while (!nodes2add.empty())
    {
        qmdd::node_handle n = nodes2add.back();
        nodes2add.pop_back();

        if (added.find(n) != end(added))
            continue;

        qmdd::node_handle children[p * p];
        dd.get_children(n, children);

        qmdd::weight_handle weights[p * p];
        dd.get_weights(n, weights);

        for (int child_idx = 0; child_idx < p * p; child_idx++)
        {
            qmdd::node_handle child = children[child_idx];

            if (declared.insert(child).second)
            {
                if (child == true_node)
                {
                    fprintf(f, "  n%u [label=\"1\",shape=box];\n", child.value);
                }
                else
                {
                    fprintf(f, "  n%u [label=\"%s\",shape=circle];\n", child.value, spec.variable_names[dd.get_var(child)].c_str());
                }
            }

            if (added.find(child) == end(added))
                nodes2add.push_back(child);
        }

        // "invisible" row of nodes for the child weights, then point those invisible nodes to the real nodes.
        fprintf(f, "  subgraph c%u {\n", n.value);
        {
            fprintf(f, "    rank=same;\n");
            fprintf(f, "    edge[style=invisible,dir=none];\n");

            for (int i = 0; i < p * p; i++)
            {
                const char* color = i % 2 == 0 ? "red" : "black";

                if (weights[i] == qmdd::weight_0_handle)
                {
                    fprintf(f, "    c%u_%d[shape=point,color=%s];\n", n.value, i, color);
                }
                else
                {
                    fprintf(f, "    c%u_%d[shape=point,width=0.01,height=0.01,color=%s];\n", n.value, i, color);
                }
            }

            // used for rank order...
            fprintf(f, "    c%u_%d[shape=point,width=0,height=0,style=invis];\n", n.value, p*p);

            for (int i = 0; i <= p * p; i++)
            {
                if (i == 0)
                    fprintf(f, "    ");
                else
                    fprintf(f, " -> ");

                if (i == p*p / 2)
                {
                    // putting the invisible node here make the children more centered
                    fprintf(f, "c%u_%d", n.value, p*p);
                }
                else if (i > p*p / 2)
                {
                    fprintf(f, "c%u_%d", n.value, i - 1);
                }
                else
                {
                    fprintf(f, "c%u_%d", n.value, i);
                }
            }

            fprintf(f, ";\n");
        }
        fprintf(f, "  }\n");

        for (int i = 0; i < p * p; i++)
        {
            const char* color = i % 2 == 0 ? "red" : "black";
            fprintf(f, "  n%u -> c%u_%d [label=\"%s\", arrowhead=none,color=%s,fontcolor=%s];\n", n.value, n.value, i, dd.to_string(weights[i]).c_str(), color, color);
        }

        for (int i = 0; i < p * p; i++)
        {
            if (weights[i] == qmdd::weight_0_handle)
            {
                continue;
            }

            const char* color = i % 2 == 0 ? "red" : "black";
            fprintf(f, "  c%u_%d -> n%u [constraint=false,color=%s];\n", n.value, i, children[i].value, color);
            fprintf(f, "  c%u_%d -> n%u [style=invis];\n", n.value, p*p, children[i].value);
        }

        added.insert(n);
    }

    fprintf(f, "}\n");

    fclose(f);
}

void display_dot(const char* fn)
{
    std::string dotcmd = std::string("packages\\Graphviz.2.38.0.2\\dot.exe") + " -Tpng " + fn + " -o " + fn + ".png";
    if (system(dotcmd.c_str()) == 0)
    {
        std::string pngcmd = std::string(fn) + ".png";
        system(pngcmd.c_str());
    }
}

int main(int argc, char* argv[]) try
{
    if (argc < 2)
    {
        printf("Usage: %s <input>\n", argc == 0 ? "qmdd" : argv[0]);
        return 0;
    }

    const char* infilename = argv[1];

    std::ifstream infile(infilename);
    if (!infile)
    {
        throw std::runtime_error(std::string("failed to open ") + infilename);
    }

    // reads the whole file into a string. Total C++ nonsense, but it works.
    std::string spec_str(std::istreambuf_iterator<char>{infile}, std::istreambuf_iterator<char>{});

    program_spec spec;
    try {
        spec = parse(spec_str.c_str());
    }
    catch (const std::exception& e) {
        throw std::runtime_error(std::string(infilename) + ":" + e.what());
    }

    qmdd::edge root;
    qmdd dd = decode(spec, &root);

    std::string outfilename = std::string(infilename) + ".dot";
    
    write_dot(infilename, spec, dd, root, outfilename.c_str());

    display_dot(outfilename.c_str());

    return 0;
}
catch (const std::exception& e)
{
    printf("%s\n", e.what());
    return -1;
}
