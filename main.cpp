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
    fredkin
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
                        if (!has_constant_input_listing)
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

                    // Toffoli gate
                    if (std::toupper(*s) == 'T')
                    {
                        s++;
                        int pcnt;
                        s = accept_paramcount(s, &pcnt);
                        s = opt_ws(s);

                        spec.gate_stream.push_back((int)gate_opcode::toffoli);
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

                    // Fredkin gate
                    if (std::toupper(*s) == 'F')
                    {
                        s++;
                        int pcnt;
                        s = accept_paramcount(s, &pcnt);
                        s = opt_ws(s);

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

    struct edge
    {
        weight_handle w;
        node_handle v;
    };
    
    static constexpr node_handle invalid_node = node_handle{ uint32_t(-1) };
    static constexpr weight_handle invalid_weight = weight_handle{ uint32_t(-1) };

    static constexpr weight_handle weight_0_handle = weight_handle{0};
    static constexpr weight_handle weight_1_handle = weight_handle{1};

    enum edge_op
    {
        edge_op_add,
        edge_op_mul,
        edge_op_kro
    };

private:
    class weight
    {
        // rational algorithms from Boost.Rational (see boost/rational.hpp for explanation)
        class rational
        {
            int num;
            int den;

            static int gcd(int a, int b)
            {
                return b == 0 ? a : gcd(b, a % b);
            }

        public:
            rational() = default;

            rational(int n)
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

        rational real;
        rational imag;

    public:
        weight() = default;

        explicit weight(int n)
            : real(n)
            , imag(0)
        { }

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
            rational a = real, b = imag, c = other.real, d = other.imag;

            real = a + c;
            imag = b + d;

            return *this;
        }

        weight operator+(const weight& other) const
        {
            weight tmp(*this);
            return tmp += other;
        }

        weight& operator*=(const weight& other)
        {
            // (a + bi) * (c + di)
            // = ac + adi + bci - bd
            // = (ac - bd) + (ad + bc)i
            rational a = real, b = imag, c = other.real, d = other.imag;
            
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
            rational a = real, b = imag, c = other.real, d = other.imag;

            rational denom = c * c + d * d;
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
            std::string s;

            if (real.denominator() == 1)
            {
                s += std::to_string(real.numerator());
            }
            else
            {
                s += std::to_string(real.numerator()) + "/" + std::to_string(real.denominator());
            }

            if (imag.numerator() != 0)
            {
                s += "+";

                if (imag.denominator() == 1)
                {
                    s += std::to_string(imag.numerator()) + "i";
                }
                else
                {
                    s += std::to_string(imag.numerator()) + "/" + std::to_string(imag.denominator());
                }
            }

            return s;
        }
    };

    class unique_table
    {
        struct node
        {
            uint32_t var;
            std::array<node_handle,4> children;
            std::array<weight_handle,4> weights;

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
            true_node->children[0] = to_handle(true_node);
            true_node->children[1] = to_handle(true_node);
            true_node->children[2] = to_handle(true_node);
            true_node->children[3] = to_handle(true_node);
            true_node->weights[0] = weight_1_handle;
            true_node->weights[1] = weight_1_handle;
            true_node->weights[2] = weight_1_handle;
            true_node->weights[3] = weight_1_handle;
        }

        node_handle get_true() const
        {
            return to_handle(true_node);
        }

        int get_var(node_handle h) const
        {
            return to_node(h)->var;
        }

        void get_children(node_handle h, node_handle children[4]) const
        {
            const node* n = to_node(h);
            for (int i = 0; i < 4; i++)
            {
                children[i] = n->children[i];
            }
        }

        node_handle get_child(node_handle h, int i) const
        {
            return to_node(h)->children[i];
        }

        void get_weights(node_handle h, weight_handle weights[4]) const
        {
            const node* n = to_node(h);
            for (int i = 0; i < 4; i++)
            {
                weights[i] = n->weights[i];
            }
        }

        weight_handle get_weight(node_handle h, int i) const
        {
            return to_node(h)->weights[i];
        }

        node_handle insert(uint32_t var, const node_handle children[4], const weight_handle weights[4])
        {
            node n;
            n.var = var;
            n.children[0] = children[0];
            n.children[1] = children[1];
            n.children[2] = children[2];
            n.children[3] = children[3];
            n.weights[0] = weights[0];
            n.weights[1] = weights[1];
            n.weights[2] = weights[2];
            n.weights[3] = weights[3];

            uint32_t key = var;
            for (int i = 0; i < 4; i++)
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
    public:
        edge find(const edge& e0, const edge& e1, edge_op op) const
        {
            // TODO
            return edge{ invalid_weight, invalid_node };
        }

        void insert(const edge& e0, const edge& e1, edge_op op, const edge& r)
        {
            // TODO
        }
    };

    class unique_weights
    {
        std::vector<weight> weights;

    public:
        unique_weights()
        {
            weights.push_back(weight(0));
            weights.push_back(weight(1));
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

    enum weight_op
    {
        weight_op_add,
        weight_op_mul,
        weight_op_div
    };

    class computed_weights
    {
    public:
        weight_handle find(weight_handle w0, weight_handle w1, weight_op op) const
        {
            // TODO
            return invalid_weight;
        }

        void insert(weight_handle w0, weight_handle w1, weight_op op, weight_handle r)
        {
            // TODO
        }
    };

    unique_table uniquetb;
    computed_table computedtb;

    unique_weights uniquewt;
    computed_weights computedwt;

    node_handle true_node;

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
        if (children[0] == children[1] &&
            children[1] == children[2] &&
            children[2] == children[3] &&
            weights[0] == weights[1] &&
            weights[1] == weights[2] &&
            weights[2] == weights[3])
        {
            return children[0];
        }

        // enforce uniqueness constraint of QMDD
        return uniquetb.insert(var, children, weights);
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
                return edge{ dd->uniquetb.get_weight(e.v, i), dd->uniquetb.get_child(e.v, i) };
            }

            bool term(const edge& e)
            { 
                return e.v == dd->get_true();
            }

            weight_handle normalize(weight_handle children[4])
            {
                for (int i = 0; i < 4; i++)
                {
                    if (children[i] != weight_0_handle)
                    {
                        weight_handle edge_weight = children[i];

                        // normalize this child
                        children[i] = weight_1_handle;

                        // normalize the other children
                        for (int j = i + 1; j < 4; j++)
                        {
                            if (children[j] != weight_0_handle)
                            {
                                children[j] = dd->apply(children[j], edge_weight, weight_op_div);
                            }
                        }

                        return edge_weight;
                    }
                }

                assert(!"expected at least one non-zero weight");
                return weight_0_handle;
            }

            edge kro(const edge& e0, const edge& e1)
            {
                // kronecker with 0 weight gives 0
                if (w(e0) == weight_0_handle)
                    return e0;
                else if (w(e1) == weight_0_handle)
                    return e1;

                // kronecker with the "1" terminal as a base case
                if (term(e0))
                {
                    if (w(e0) == weight_1_handle)
                        return e1;
                    else
                        return edge{ dd->apply(w(e0), w(e1), weight_op_mul), v(e1) };
                }
                else if (term(e1))
                {
                    if (w(e1) == weight_1_handle)
                        return e0;
                    else
                        return edge{ dd->apply(w(e0), w(e1), weight_op_mul), v(e0) };
                }
                
                node_handle z_children[4];
                weight_handle z_weights[4];
                for (int i = 0; i < 4; i++)
                {
                    edge zi = dd->apply(Ei(e0, i), e1, edge_op_kro);
                    z_children[i] = zi.v;
                    z_weights[i] = zi.w;
                }

                weight_handle new_weight = normalize(z_weights);
                new_weight = dd->apply(new_weight, w(e0), weight_op_mul);

                node_handle new_node = dd->make_node(x(e0), z_children, z_weights);

                return edge{ new_weight, new_node };
            };
        };

        edge found = computedtb.find(e0, e1, op);
        if (found.v != invalid_node)
        {
            return found;
        }

        apply_helper helper{ this };

        edge new_edge;

        if (op == edge_op_kro)
        {
            new_edge = helper.kro(e0, e1);
        }
        else
        {
            assert(!"unimplemented op");
            return edge{ invalid_weight, invalid_node };
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
    qmdd dd(spec.num_variables);

    qmdd::node_handle true_node = dd.get_true();

    const qmdd::node_handle identity_children[4]  = { true_node,             true_node,             true_node,             true_node             };
    const qmdd::weight_handle identity_weights[4] = { qmdd::weight_1_handle, qmdd::weight_0_handle, qmdd::weight_0_handle, qmdd::weight_1_handle };
    const qmdd::weight_handle not_weights[4]      = { qmdd::weight_0_handle, qmdd::weight_1_handle, qmdd::weight_1_handle, qmdd::weight_0_handle };
    const qmdd::weight_handle if_false_weights[4] = { qmdd::weight_1_handle, qmdd::weight_0_handle, qmdd::weight_0_handle, qmdd::weight_0_handle };
    const qmdd::weight_handle if_true_weights[4]  = { qmdd::weight_0_handle, qmdd::weight_0_handle, qmdd::weight_0_handle, qmdd::weight_1_handle };

    // initialize circuit with p^n by p^n identity
    qmdd::edge root = qmdd::edge{ qmdd::weight_1_handle, true_node };
    for (int var_id = spec.num_variables - 1; var_id >= 0; var_id--)
    {
        qmdd::edge curr_identity{ qmdd::weight_1_handle, dd.make_node(var_id, identity_children, identity_weights) };

        root = dd.apply(curr_identity, root, qmdd::edge_op_kro);
    }

    for (int gate_stream_idx = 0; gate_stream_idx < spec.gate_stream.size(); )
    {
        gate_opcode opcode = (gate_opcode)spec.gate_stream[gate_stream_idx];
        gate_stream_idx++;

        int param_count = spec.gate_stream[gate_stream_idx];
        gate_stream_idx++;
        
        const int* first_param = &spec.gate_stream[gate_stream_idx];
        const int* last_param = &spec.gate_stream[gate_stream_idx] + param_count;

        switch (opcode)
        {
        case gate_opcode::toffoli:
        {
#ifdef SHOW_INSTRS
            printf("t%d ", param_count);
            for (const int* p = first_param; p < last_param; p++)
            {
                if (p != first_param)
                {
                    printf(",");
                }
                printf("%s", spec.variable_names[*p].c_str());
            }
            printf("\n");
#endif

            assert(last_param - first_param >= 1);

            int target_var_id = *(last_param - 1);
            
            qmdd::node_handle target_identity_handle = dd.make_node(target_var_id, identity_children, identity_weights);
            qmdd::node_handle target_not_handle = dd.make_node(target_var_id, identity_children, not_weights);

            for (int var_id = spec.num_variables - 1; var_id >= 0; var_id--)
            {
                // variables below the target
                if (var_id > target_var_id)
                {
                    continue;
                }

                // the target variable
                if (var_id == target_var_id)
                {
                    // TODO: point children to QMDDs constructed below it
                    root = qmdd::edge{ qmdd::weight_1_handle, dd.make_node(target_var_id, identity_children, not_weights) };
                    continue;
                }

                // variables above the target
                if (var_id < target_var_id)
                {
                    continue;
                }
            }

            break;
        }
        case gate_opcode::fredkin:
        {
#ifdef SHOW_INSTRS
            printf("f%d ", param_count);
            for (const int* p = first_param; p < last_param; p++)
            {
                if (p != first_param)
                {
                    printf(",");
                }
                printf("%s", spec.variable_names[*p].c_str());
            }
            printf("\n");
#endif

            assert(!"fredkin not supported yet");

            break;
        }
        default:
            throw std::logic_error("unknown gate opcode");
        }

        gate_stream_idx += param_count;
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
    FILE* f = fopen(fn, "w");

    if (!f)
    {
        throw std::runtime_error(std::string("failed to open ") + fn);
    }

    fprintf(f, "digraph {\n");

    fprintf(f, "  labelloc=\"t\";\n");
    fprintf(f, "  label=\"%s\";\n", title);

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
    fprintf(f, "  root -> n%x [label=\"%s\"];\n", root.v.value, dd.to_string(root.w).c_str());

    if (root.v == true_node)
    {
        fprintf(f, "  n%x [label=\"1\",shape=box];\n", true_node.value);
    }
    else
    {
        fprintf(f, "  n%x [label=\"%s\",shape=circle];\n", root.v.value, spec.variable_names[dd.get_var(root.v)].c_str());
    }

    declared.insert(root.v);

    while (!nodes2add.empty())
    {
        qmdd::node_handle n = nodes2add.back();
        nodes2add.pop_back();

        if (added.find(n) != end(added))
            continue;

        qmdd::node_handle children[4];
        dd.get_children(n, children);

        qmdd::weight_handle weights[4];
        dd.get_weights(n, weights);

        for (int child_idx = 0; child_idx < 4; child_idx++)
        {
            qmdd::node_handle child = children[child_idx];

            if (declared.insert(child).second)
            {
                if (child == true_node)
                {
                    fprintf(f, "  n%x [label=\"1\",shape=box];\n", true_node.value);
                }
                else
                {
                    fprintf(f, "  n%x [label=\"%s\",shape=circle];\n", child.value, spec.variable_names[dd.get_var(child)].c_str());
                }
            }

            if (added.find(child) == end(added))
                nodes2add.push_back(child);
        }

        // "invisible" row of nodes for the child weights, then point those invisible nodes to the real nodes.
        fprintf(f, "  subgraph c%x {\n", n.value);
        {
            fprintf(f, "    rank=same;\n");
            fprintf(f, "    edge[style=invisible,dir=none];\n");

            for (int i = 0; i < 4; i++)
            {
                if (weights[i] == qmdd::weight_0_handle)
                {
                    fprintf(f, "    c%x_%d[shape=point];\n", n.value, i);
                }
                else
                {
                    fprintf(f, "    c%x_%d[shape=point,width=0.01,height=0.01];\n", n.value, i);
                }
            }

            for (int i = 0; i < 4; i++)
            {
                if (i == 0)
                    fprintf(f, "    ");
                else
                    fprintf(f, " -> ");

                fprintf(f, "c%x_%d", n.value, i);
            }
            fprintf(f, ";\n");
        }
        fprintf(f, "  }\n");

        for (int i = 0; i < 4; i++)
        {
            fprintf(f, "  n%x -> c%x_%d [label=\"%s\", arrowhead=none];\n", n.value, n.value, i, dd.to_string(weights[i]).c_str());
        }

        for (int i = 0; i < 4; i++)
        {
            if (weights[i] == qmdd::weight_0_handle)
            {
                continue;
            }

            fprintf(f, "  c%x_%d -> n%x;\n", n.value, i, children[i].value);
        }

        added.insert(n);
    }

    fprintf(f, "}\n");

    fclose(f);

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
        printf("Failed to open %s\n", infilename);
        return -1;
    }

    // reads the whole file into a string. Total C++ nonsense, but it works.
    std::string spec_str(std::istreambuf_iterator<char>{infile}, std::istreambuf_iterator<char>{});

    program_spec spec;
    try
    {
        spec = parse(spec_str.c_str());
    }
    catch (const std::exception& e)
    {
        throw std::runtime_error(std::string(infilename) + ":" + e.what());
    }

    qmdd::edge root;
    qmdd dd = decode(spec, &root);

    std::string outfilename = std::string(infilename) + ".dot";
    write_dot(infilename, spec, dd, root, outfilename.c_str());
}
catch (const std::exception& e)
{
    printf("%s\n", e.what());
    return -1;
}
