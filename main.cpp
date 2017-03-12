#include <string>
#include <fstream>
#include <vector>
#include <map>
#include <cstdio>
#include <cctype>

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
    std::vector<bool> variable_is_input;
    std::vector<bool> variable_is_output;
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

void decode(const program_spec& spec)
{
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
            break;
        }
        default:
            throw std::logic_error("unknown gate opcode");
        }

        gate_stream_idx += param_count;
    }
}

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
                            // TODO: check that every input is satisified
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

                        spec.variable_is_input.resize(spec.num_variables, false);
                        spec.variable_is_output.resize(spec.num_variables, false);
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

                            if (spec.variable_is_input[it->second])
                            {
                                throw std::runtime_error("duplicate input");
                            }

                            spec.variable_is_input[it->second] = true;

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

                            if (spec.variable_is_output[it->second])
                            {
                                throw std::runtime_error("duplicate output");
                            }

                            spec.variable_is_output[it->second] = true;

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

                                if (!spec.variable_is_input[curr_input_var_id])
                                {
                                    break;
                                }
                                
                                curr_input_var_id += 1;
                            }

                            spec.variable_constant_input[curr_input_var_id] = cval;
                        });

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

int main(int argc, char* argv[])
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

    program_spec spec = parse(spec_str.c_str());

    decode(spec);
}
