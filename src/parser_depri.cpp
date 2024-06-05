#include "parser.hpp"

#include <cstdint>

#include "lexer.hpp"


const char* Ops_name_table[int(Ops::SIZE)] {
    "+", "-", "*", "/",
    "==", "!=", "!>", "!<", ">=", "<=",
    "&&", "||",
    "none", "ghost", "permutable", "array",
    "{", "(", "<", "[", "\'",
    ">.", "^.", ">..", "^..", "##",
    "%", "&",
    "vert_line", "equal", "circumflex",
    "dot", "at", "questionm", "exclamm"
};

    const std::array<Ops, Token_enum::SIZE> tkn_to_op_table = get_tkn_to_op_table();

    enum class Obj_class : unsigned char {
        None,

        Global_space,  // Only the root node has this type.
        Base_name,     // Base_names are the supers of Base_types, who carry the external name of that Base_type.
        Base_type,     // Base_types are the building blocks for other more complex types. Their behaviour is defined in an extention of the compiler.
        Meta_template, // Meta_templates, similarly to Base_types, have a corresponding representation in an extension of the compiler.
        Template,      // Templates are subs of Meta_templates as they "fit" insider their super as in, they cannot introduce new ambiguity.
        Object,        // Objects are fully constrained/defined Templates.

        Reference,     // References are "empty" nodes that only link to a super. The super can be any node except Reference, including "Virtual_group".
        Virtual_group  // Virtual_groups are otherwise "empty" nodes whose downs represent a set of nodes meeting certain criteria.
    };

    struct Ast_node {
        Ast_node(uint32_t tkn_idx, Obj_class obj_class = Obj_class::None, Ops op = Ops::None)
            : tkn_idx(tkn_idx), node_class(obj_class), op(op) {}
        Ast_node(Ast_node* up, uint32_t tkn_idx, Obj_class obj_class = Obj_class::None, Ops op = Ops::None)
            : up(up), on_based_branch(up->on_based_branch), permutable_node(up->permutable_node), tkn_idx(tkn_idx), node_class(obj_class), op(op) {}
        Ast_node(const Ast_node& other) { *this = other; };
        //Ast_node& operator=(const Ast_node& other) = delete;
        ~Ast_node() {
            if (downs) {
                for (uint32_t i = 0; i < n_downs; ++i)
                    delete downs[i];
                delete[] downs;
            }
            if (node_class == Obj_class::Base_type || node_class == Obj_class::Meta_template)
                delete super; // this would be the name containig node, owned by this node.
            if (node_class == Obj_class::Reference) {
                delete super;
                if (subs) {
                    for (uint32_t i = 0; i < n_subs; ++i)
                        delete subs[i];
                    delete[] subs;
                }
            }
        }

        std::string get_ident_string(const std::vector<Token>& tokens_ref) const {
            Ast_node* new_up = up;
            std::string ident;
            if (tokens_ref[tkn_idx].type == tkn_ident)
                ident = tokens_ref[tkn_idx].s_v;
            for (new_up; new_up != nullptr; new_up = new_up->up) {
                if (tokens_ref[new_up->tkn_idx].type == tkn_ident)
                    ident = std::string(tokens_ref[new_up->tkn_idx].s_v) + " >. " + ident;
            }
            return ident;
        }

        Ops op = Ops::None;
        bool on_based_branch = false;
        bool permutable_node = false;
        Obj_class node_class = Obj_class::None;

        Ast_node* super = nullptr;
        Ast_node** subs = nullptr;
        Ast_node* up = nullptr;
        Ast_node** downs = nullptr;
        uint16_t n_downs = 0;
        uint16_t n_subs = 0;
        uint32_t tkn_idx = 0;
    };

    struct Ast {

        Ast(const std::vector<Token>& tokens_ref)
            : tokens_ref{ tokens_ref }, ident_lookup{ 1, tokens_ref } {}

        bool add_to_ident_lookup(Ast_node* node) {
            // Using node->up with s_v of node, for performance optimization. As this makes it compatible with Ident_ref_ptr,
            // which is used for identifier lookup.
            auto result = ident_lookup.emplace(Ident_ref_ptr{ node->up, tokens_ref[node->tkn_idx].s_v, tokens_ref }, node);
            return result.second;
        }

        struct Ident_ref_ptr {
            Ident_ref_ptr(const Ast_node* up_node, std::string_view s_v, const std::vector<Token>& tokens_ref)
                : up_node(up_node), s_v(s_v), tokens_ref(tokens_ref) {}
            bool operator==(const Ident_ref_ptr& other) const {
                if (s_v != other.s_v)
                    return false;
                else {
                    Ast_node* up1 = up_node->up, * up2 = other.up_node->up;
                    // checking for up1->up is equivalent to checking whether up1 is not the global space node.
                    for (up1, up2; up1 && up2 && up1->up && up2->up; up1 = up1->up, up2 = up2->up) {

                        // Brauchen das hier wahrscheinlich später !!!

                        //while (tokens_ref[up1->tkn_idx].type != tkn_ident && up1->up)
                        //    up1 = up1->up;
                        //while (tokens_ref[up2->tkn_idx].type != tkn_ident && up2->up)
                        //    up2 = up2->up;
                        //if (up1 && up2 && tokens_ref[up1->tkn_idx].type == tkn_ident && tokens_ref[up2->tkn_idx].type == tkn_ident) {
                        if (tokens_ref[up1->tkn_idx].s_v != tokens_ref[up2->tkn_idx].s_v)
                            return false;
                        else
                            continue;
                        //}
                        //else
                        //    return false;
                    }
                    if ((!up1) ^ (!up2))
                        return false;
                    return true;
                }
            }
            const Ast_node* up_node;
            std::string_view s_v;
            const std::vector<Token>& tokens_ref;
        };

        struct Ident_ref_ptr_hash {
            Ident_ref_ptr_hash(const std::vector<Token>& tokens_ref) : tokens_ref(tokens_ref) {}

            size_t operator()(const Ident_ref_ptr& ident_ref_ptr) const {
                size_t hash = std::hash<std::string_view>{}(ident_ref_ptr.s_v);
                for (const Ast_node* up = ident_ref_ptr.up_node; up != nullptr; up = up->up)
                    if (tokens_ref[up->tkn_idx].type == tkn_ident)
                        hash ^= std::hash<std::string_view>{}(tokens_ref[up->tkn_idx].s_v);
                return hash;
            }
            const std::vector<Token>& tokens_ref;
        };

        Ast_node* find_up(const Ast_node* up_node, std::string_view ident_s_v) {
#ifdef TRACY_ALL
            ZoneScoped;
            ZoneColor(tracy::Color::LightSalmon2);
#endif
            decltype(ident_lookup)::iterator itr;
            auto search = [&](const Ast_node* scope) -> bool {
#ifdef TRACY_ALL
                ZoneScopedN("ident_search_lambda");
#endif
                itr = ident_lookup.find({ scope, ident_s_v, tokens_ref });
                return itr != ident_lookup.end();
            };

            const Ast_node* up = up_node;
            for (;;) {
                if (search(up))
                    return itr->second;

                if (up->super) {
                    const Ast_node* super = up->super;
                    for (;;) {
                        if (search(super))
                            return itr->second;

                        if (super->super)
                            super = super->super;
                        else
                            break;
                    }
                }
                if (up->up)
                    up = up->up;
                else
                    return nullptr;
            }
        }

        // find_down() can only go a single step
        Ast_node* find_down(const Ast_node* up_node, std::string_view ident_s_v) {
            decltype(ident_lookup)::iterator itr;
            itr = ident_lookup.find({ up_node, ident_s_v, tokens_ref });
            if (itr != ident_lookup.end())
                return itr->second;
            else
                return nullptr;
        }

        void print_idents() const {
            std::cout << "all saved identifiers:\nline:offset identifier\n\n";
            for (const auto& [i, a] : ident_lookup) {
                std::cout << tokens_ref[a->tkn_idx].line_pos << ':' << tokens_ref[a->tkn_idx].h_pos
                    << '\t' << i.up_node->get_ident_string(tokens_ref) << " >. " << i.s_v << '\n';
            }
            std::cout << '\n' << ident_lookup.size() << " idents in total\n";
        }

        void write_subs_to_supers() {
#ifdef TRACY_ALL
            ZoneScoped;
#endif
            for (auto const& [node, subs_v] : super_to_subs_map) {
                node->n_subs = (uint16_t)subs_v.size();
                node->subs = new Ast_node * [node->n_subs];
                for (int i = 0; i < node->n_subs; ++i)
                    node->subs[i] = subs_v[i];
            }
        }

        const std::vector<Token>& tokens_ref;
        std::unordered_map<Ident_ref_ptr, Ast_node*, Ident_ref_ptr_hash> ident_lookup;
        std::unordered_map<Ast_node*, std::vector<Ast_node*>> super_to_subs_map;
        Ast_node root_node{ 0, Obj_class::Global_space, Ops::None };
    };

    class Parser {
    public:

        Ast* parse(const char* parse_path) {

#ifdef TRACY_CORSE
            ZoneScoped;
#endif
            lexer.tokenize(lexer_source::from_directory, parse_path);
#ifdef KE_DEBUG
            lexer.print_all_tokens(true, true);
            if (error_stack.print(true))
                return nullptr;
#endif
            ast = new Ast{ lexer.tokens_ref() };

            t_begin = lexer.tokens_ref().begin();
            t_end = lexer.tokens_ref().end();
            t_global_scope_progress = t_begin;

            parse_node_recursive(&ast->root_node, false, 0, false, true, &global_downs_v);
            ast->write_subs_to_supers();
            
            collect_references();

#ifdef KE_DEBUG
            print_ast();
            ast->print_idents();
#endif
            if (!error_stack.print(true))
                return ast;
            else
                return nullptr;
        }

        void print_ast() {
            std::cout << "\"Abstract\" syntax tree:\n\n";
            print_ast_recursive(&ast->root_node);
            std::cout << '\n';
        }

        void print_ast_recursive(Ast_node* node, int depth = 0) {
            if (!node)
                return;
            if (depth == 1)
                std::cout << '\n';
            for (int i = 0; i < depth; ++i)
                std::cout << "   ";
            if (node->on_based_branch)
                std::cout << "\033[95mB \033[0m";
            if (node->permutable_node)
                std::cout << "\033[93mP \033[0m";

            std::cout << "\033[96m";
            switch (node->node_class) {
            case Obj_class::Base_type: std::cout << "<base_type> "; break;
            case Obj_class::Meta_template: std::cout << "<meta_template> "; break;
            case Obj_class::Template: std::cout << "<template> "; break;
            case Obj_class::Object: std::cout << "<object> "; break;
            case Obj_class::Reference: std::cout << "<ref> "; break;
            case Obj_class::Virtual_group: std::cout << "<virtual_group> "; break;
            }
            std::cout << "\033[0m";

            if (lexer.tokens_ref()[node->tkn_idx].type == tkn_ident)
                std::cout << "\033[91m" << lexer.tokens_ref()[node->tkn_idx].s_v << "\033[0m";
            else if (node->op != Ops::None)
                std::cout << "\033[92m" << Ops_name_table[int(node->op)] << "\033[0m";
            else {
                Token_enum tkn = lexer.tokens_ref()[node->tkn_idx].type;
                std::cout << "\033[94m";
                switch (tkn) {
                case tkn_ident:
                case tkn_string: std::cout << lexer.tokens_ref()[node->tkn_idx].s_v; break;
                case tkn_int: std::cout << lexer.tokens_ref()[node->tkn_idx].i; break;
                case tkn_real: std::cout << lexer.tokens_ref()[node->tkn_idx].d; break;
                default:
                    if (tkn < 256)
                        std::cout << (char)tkn;
                    else if (tkn < Token_enum::SIZE)
                        std::cout << token_enum_name_table[tkn - 256];
                }
                std::cout << "\033[0m";
            }
            if (node->up)
                std::cout << "\033[91m" << " . " << "\033[0m" << node->up->get_ident_string(lexer.tokens_ref());
            if (node->super)
                std::cout << "\033[93m" << " < " << "\033[0m" << node->super->get_ident_string(lexer.tokens_ref());
            if (node->n_subs) {
                std::cout << "\033[31m" << " > " << "\033[0m";
                for (int i = 0; i < node->n_subs; ++i)
                    if (node->subs[i] && lexer.tokens_ref()[node->subs[i]->tkn_idx].type == tkn_ident)
                        std::cout << " " << lexer.tokens_ref()[node->subs[i]->tkn_idx].s_v;
                    else std::cout << " no_ident?? ";
            }
            std::cout << '\n';
            for (uint32_t i = 0; i < node->n_downs; ++i)
                print_ast_recursive(node->downs[i], depth + 1);
        }

        void parsing_error(const Ast_node* node, const char* msg) {
            lexer.parsing_error(node->tkn_idx, msg);
        }

    private:

        Lexer lexer;
        Ast* ast = nullptr;
        std::vector<Ast_node*> global_downs_v, nodes_with_math;
        std::vector<Token>::iterator t_begin, t_end, t_global_scope_progress;

        std::vector<Ast_node*> reference_nodes_v;

        bool collect_ref(Ast_node* up, Ast_node* ref_arg, std::vector<Ast_node*>& refs_v, std::vector<Ast_node*>& swap_refs_v) {

            if (ref_arg->n_downs) {
                if (!collect_ref(up, ref_arg->downs[0], refs_v, swap_refs_v))
                    return false;

                if (ref_arg->op == Ops::Access_down || ref_arg->op == Ops::Access_sub || ref_arg->op == Ops::And || ref_arg->op == Ops::Or) {
                    if (ref_arg->n_downs != 2) {
                        lexer.parsing_error(t_begin + ref_arg->tkn_idx, "Expected two arguments");
                        return false;
                    }
                }

                switch (ref_arg->op) {
                case Ops::Access_down:
                    if ((t_begin + ref_arg->downs[1]->tkn_idx)->type == tkn_ident) {
                        for (const auto& ref : refs_v)
                            swap_refs_v.push_back(ast->find_down(ref, (t_begin + ref_arg->downs[1]->tkn_idx)->s_v));
                    }
                    else {
                        lexer.parsing_error(t_begin + ref_arg->tkn_idx, "Expected an identifier.");
                        return false;
                    }
                    break;
                case Ops::Access_downs:
                    for (const auto& ref : refs_v) {
                        for (int i = 0; i < ref->n_downs; ++i)
                            swap_refs_v.push_back(ref->downs[i]);
                    }
                    break;
                case Ops::Access_sub:
                    if ((t_begin + ref_arg->subs[1]->tkn_idx)->type == tkn_ident) {
                        for (const auto& ref : refs_v)
                            if ((t_begin + ref->tkn_idx)->type == tkn_ident && (t_begin + ref_arg->subs[1]->tkn_idx)->s_v == (t_begin + ref->tkn_idx)->s_v)
                                swap_refs_v.push_back(ref);
                    }
                    else {
                        lexer.parsing_error(t_begin + ref_arg->tkn_idx, "Expected an identifier.");
                        return false;
                    }
                    break;
                case Ops::Access_subs:
                    for (const auto& ref : refs_v) {
                        for (int i = 0; i < ref->n_subs; ++i)
                            swap_refs_v.push_back(ref->subs[i]);
                    }
                    break;
                case Ops::And:
                {
                    std::vector<Ast_node*> refs_branch_v, swap_refs_branch_v;
                    if (!collect_ref(up, ref_arg->downs[1], refs_branch_v, swap_refs_branch_v))
                        return false;
                    for (const auto& ref : refs_v) {
                        for (const auto& ref_branch : refs_branch_v)
                            if (ref == ref_branch)
                                swap_refs_v.push_back(ref);
                    }
                }
                    break;
                case Ops::Or:
                {
                    std::vector<Ast_node*> refs_branch_v, swap_refs_branch_v;
                    if (!collect_ref(up, ref_arg->downs[1], refs_branch_v, swap_refs_branch_v))
                        return false;
                    swap_refs_v.swap(refs_v);
                    for (const auto& ref_branch : refs_branch_v) {
                        bool is_included = true;
                        for (const auto& ref : swap_refs_v) {
                            if (ref == ref_branch)
                                is_included = false;
                            swap_refs_v.push_back(ref_branch);
                        }
                    }
                }
                    break;
                }

                swap_refs_v.swap(refs_v);
                swap_refs_v.clear();

                return true;
            }
            else {
                if ((t_begin + ref_arg->tkn_idx)->type == tkn_ident) {
                    Ast_node* start_node = ast->find_up(up, (t_begin + ref_arg->tkn_idx)->s_v);
                    if (start_node)
                        refs_v.push_back(start_node);
                    else
                        lexer.parsing_error(t_begin + ref_arg->tkn_idx, "Could not find this identifier in the scope of the reference.");
                }
                else
                    lexer.parsing_error(t_begin + ref_arg->tkn_idx, "Expected an identifier.");

                return true;
            }
        }

        bool collect_references() {
            for (auto& ref_node : reference_nodes_v) {

                std::vector<Ast_node*> refs_v, swap_refs_v;
                if (ref_node->n_downs != 1) {
                    lexer.parsing_error(t_begin + ref_node->tkn_idx, "Malformed reference, n_downs != 1");
                    return false;
                }

                if (!collect_ref(ref_node->up, ref_node->downs[0], refs_v, swap_refs_v)) {
                    return false;
                }
                else if (!refs_v.empty()) {
                    if (ref_node->n_subs) { // TODO: why no error, when there already is somehting in subs????
                        for (int i = 0; i < ref_node->n_subs; ++i)
                            delete ref_node->subs[i];
                        delete[] ref_node->subs;
                    }
                    ref_node->n_subs = (uint16_t)refs_v.size();
                    ref_node->subs = new Ast_node * [ref_node->n_subs];
                    for (uint32_t i = 0; i < ref_node->n_subs; ++i)
                        ref_node->subs[i] = refs_v[i];
                }
                //else {
                //    lexer.parsing_error(t_begin + ref_node->tkn_idx, "Found nothing to reference.");
                //}
            }
        }

        // bool is for error, booleans are stored in int also.
        std::variant<bool, int64_t, double> eval_const_math(Ast_node* expr_node) {
            using value_t = std::variant<bool, int64_t, double>;
            if (!expr_node)
                return false;
            if (is_arithmetic_op(t_begin + expr_node->tkn_idx)) {
                if (expr_node->n_downs == 2) { // all math ops are currently binary
                    auto l_v = eval_const_math(expr_node->downs[0]);
                    auto r_v = eval_const_math(expr_node->downs[1]);
                    if (l_v.index() == 0 || r_v.index() == 0)
                        return false;
                    switch (expr_node->op) {
                    case Ops::Add:
                        return std::visit([](auto& l, auto& r) -> value_t {return l + r; }, l_v, r_v);
                        break;
                    case Ops::Sub:
                        return std::visit([](auto& l, auto& r) -> value_t {return l - r; }, l_v, r_v);
                        break;
                    case Ops::Mul:
                        return std::visit([](auto& l, auto& r) -> value_t {return l * r; }, l_v, r_v);
                        break;
                    case Ops::Div:
                        return std::visit([](auto& l, auto& r) -> value_t {return l / r; }, l_v, r_v);
                        break;
                    case Ops::Is_eq:
                        return std::visit([](auto& l, auto& r) {return int64_t(l == r); }, l_v, r_v);
                        break;
                    case Ops::Is_neq:
                        return std::visit([](auto& l, auto& r) {return int64_t(l != r); }, l_v, r_v);
                        break;
                    case Ops::Is_greater:
                        return std::visit([](auto& l, auto& r) {return int64_t(l > r); }, l_v, r_v);
                        break;
                    case Ops::Is_less:
                        return std::visit([](auto& l, auto& r) {return int64_t(l < r); }, l_v, r_v);
                        break;
                    case Ops::Is_greater_eq:
                        return std::visit([](auto& l, auto& r) {return int64_t(l >= r); }, l_v, r_v);
                        break;
                    case Ops::Is_less_eq:
                        return std::visit([](auto& l, auto& r) {return int64_t(l <= r); }, l_v, r_v);
                        break;
                    case Ops::And:
                        return std::visit([](auto& l, auto& r) {return int64_t(l && r); }, l_v, r_v);
                        break;
                    case Ops::Or:
                        return std::visit([](auto& l, auto& r) {return int64_t(l || r); }, l_v, r_v);
                        break;
                    default:
                        return false;
                    }
                }
                else
                    return false;
            }
            else if (expr_node->n_downs == 1 && is_opening_bracket_tkn(t_begin + expr_node->tkn_idx))
                return eval_const_math(expr_node->downs[0]);
            else if ((t_begin + expr_node->tkn_idx)->type == tkn_int)
                return (t_begin + expr_node->tkn_idx)->i;
            else if ((t_begin + expr_node->tkn_idx)->type == tkn_real)
                return (t_begin + expr_node->tkn_idx)->d;
            else
                return false;
        }

        bool find_and_parse(Ast_node* up_node, std::string_view ident_s_v, std::vector<Token>::iterator t) {
#ifdef TRACY_ALL
            ZoneScoped;
#endif
            Ast_node* up = up_node;
            if (up->node_class == Obj_class::Global_space)
                t = t_global_scope_progress + 1;
            while (t < t_end) {
                // using while to handle multiple scopes ending in t
                while (up->up && t - t_begin > lexer.tokens_ref()[up->tkn_idx].scope_end) {
                    up = up->up;
                    if (up->node_class == Obj_class::Global_space)
                        t = t_global_scope_progress + 1;
                }
                if (is_declaration_ident(t) && t->s_v == ident_s_v) {
                    bool result = parse_node_recursive(up, false, (int32_t)abs(t - t_begin - up->tkn_idx), true);
                    return result;
                }
                t = t_begin + t->scope_end + 1; // when encountering any new scope, skip it.
            }
            return false;
        }

        Ast_node* parse_expression(std::vector<Ast_node*>* downs_v, Ast_node* up, std::vector<Token>::iterator& t, vec2<uint32_t> range, bool first = false) {
            
            uint32_t lowest_p = 9999;
            uint32_t lowest_p_t = UINT32_MAX;

            // find lowest precidence operator in the range
            for (uint32_t it = range.x; it < range.y + 1; ++it) {
                if ((t_begin + it)->type == ',') {
                    range.y = it - 1;
                    break;
                }
                if (is_opening_bracket_tkn(t_begin + it)) {
                    if (op_precidence_table[(t_begin + it)->type] <= lowest_p) {
                        lowest_p_t = it;
                        lowest_p = op_precidence_table[(t_begin + it)->type];
                    }
                    while (!is_closing_bracket_tkn(t_begin + it)) ++it;
                    while (is_closing_bracket_tkn(t_begin + it + 1)) ++it;
                }
                else if (is_op(t_begin + it)) {
                    if (op_precidence_table[(t_begin + it)->type] <= lowest_p) {
                        lowest_p_t = it;
                        lowest_p = op_precidence_table[(t_begin + it)->type];
                    }
                }
            }

            if (!(range.y - range.x))
                return new Ast_node{ up, range.x };
            else if (range.y <= range.x) {
                lexer.parsing_error(t_begin + range.x, "Unexpected operator.");
                return nullptr;
            }

            Ast_node* left = nullptr, * node = nullptr, * right = nullptr;
            if (lowest_p_t != UINT32_MAX) {
                node = new Ast_node{ up, lowest_p_t, Obj_class::None, tkn_to_op_table[int((t_begin + lowest_p_t)->type)] };
            }
            else {
                lexer.parsing_error(t_begin + range.x, "Weird expression.");
                return nullptr;
            }

            if (is_binary_op(t_begin + lowest_p_t)) {
                if (range.x == lowest_p_t - 1 && first && !downs_v->empty() && is_closing_bracket_tkn(t_begin + range.x)) {
                    left = downs_v->back();
                    downs_v->pop_back();
                }
                else
                    left = parse_expression(downs_v, node, t, { range.x, lowest_p_t - 1 });
                right = parse_expression(downs_v, node, t, { lowest_p_t + 1, range.y });
                node->n_downs = 2;
                node->downs = new Ast_node * [2];
                node->downs[0] = left;
                node->downs[1] = right;
            }
            else if (is_unary_op_left_associative(t_begin + lowest_p_t)) {
                if (range.x == lowest_p_t - 1 && first && !downs_v->empty() && is_closing_bracket_tkn(t_begin + range.x)) {
                    left = downs_v->back();
                    downs_v->pop_back();
                }
                else
                    left = parse_expression(downs_v, node, t, { range.x, lowest_p_t - 1 });
                node->n_downs = 1;
                node->downs = new Ast_node * [1];
                node->downs[0] = left;
                if (lowest_p_t + 1 <= range.y && (is_binary_op(t_begin + lowest_p_t + 1) || is_unary_op_left_associative(t_begin + lowest_p_t + 1))) {
                    right = parse_expression(downs_v, node, t, { lowest_p_t + 2, range.y });
                    left = node;
                    node = new Ast_node{ up, lowest_p_t + 1, Obj_class::None, tkn_to_op_table[int((t_begin + lowest_p_t + 1)->type)] };
                    left->up = node;
                    node->n_downs = 2;
                    node->downs = new Ast_node * [2];
                    node->downs[0] = left;
                    node->downs[1] = right;
                }
            }
            else if (is_unary_op_right_associative(t_begin + lowest_p_t)) {
                if (node->op == Ops::Reference || node->op == Ops::Value_reference) {
                    node->node_class = Obj_class::Reference;
                    reference_nodes_v.push_back(node);
                }
                right = parse_expression(downs_v, node, t, { lowest_p_t + 1, range.y });
                node->n_downs = 1;
                node->downs = new Ast_node * [1];
                node->downs[0] = right;
                if (lowest_p_t - 1 >= range.x && (is_binary_op(t_begin + lowest_p_t - 1) || is_unary_op_right_associative(t_begin + lowest_p_t - 1))) {
                    if (range.x == lowest_p_t - 1 && first && !downs_v->empty() && is_closing_bracket_tkn(t_begin + range.x)) {
                        left = downs_v->back();
                        downs_v->pop_back();
                    }
                    else
                        left = parse_expression(downs_v, node, t, { range.x, lowest_p_t - 2 });
                    right = node;
                    node = new Ast_node{ up, lowest_p_t - 1, Obj_class::None, tkn_to_op_table[int((t_begin + lowest_p_t - 1)->type)] };
                    if (node->op == Ops::Reference || node->op == Ops::Value_reference) {
                        node->node_class = Obj_class::Reference;
                        reference_nodes_v.push_back(node);
                    }
                    right->up = node;
                    node->n_downs = 2;
                    node->downs = new Ast_node * [2];
                    node->downs[0] = left;
                    node->downs[1] = right;
                }
            }
            else if (is_opening_bracket_tkn(t_begin + lowest_p_t)) {
                parse_node_recursive(node, true);
            }
            else {
                lexer.parsing_error(t_begin + lowest_p_t, "Unexpected operator.");
                return nullptr;
                if (first)
                    t = t_begin + range.y + 1;
            }
            
            if (first)
                t = t_begin + range.y + 1;

            return node;
        }

        void connect_super(Ast_node* node, Ast_node* super) {
            node->super = super;
            if (node->node_class != Obj_class::Reference && node->node_class != Obj_class::None)
                ast->super_to_subs_map[super].push_back(node);
        }

        Ast_node* find_super(Ast_node* up, std::vector<Token>::iterator t, bool falure_is_error = true) {

            Ast_node* up_ref;
            if (t->type == tkn_ident) {
                up_ref = ast->find_up(up, t->s_v);
                if (!up_ref) {
                    if (!find_and_parse(up, t->s_v, t_begin + t->scope_end + 1)) {
                        if(falure_is_error)
                            lexer.parsing_error(t, "Failed trying to find super.");
                        return nullptr;
                    }
                    up_ref = ast->find_up(up, t->s_v);
                    if (!up_ref) {
                        if (falure_is_error)
                            lexer.parsing_error(t, "This identifier does not exist in the current scope.");
                        return nullptr;
                    }
                }
            }
            else {
                lexer.parsing_error(t, "Expected an identifier.");
                return nullptr;
            }

            return up_ref;
        }

        bool parse_node_recursive(Ast_node* up, bool only_downs = false, uint32_t offset = 0, bool only_this_scope = false,
            bool main_front = false, std::vector<Ast_node*> *ex_downs_v = nullptr) {

            std::vector<Token>::iterator t = t_begin + up->tkn_idx + offset, fist, second, t_super, t_node;
            std::vector<Ast_node*> downs_v_data;
            std::vector<Ast_node*> *downs_v;
            if (ex_downs_v) downs_v = ex_downs_v;
            else            downs_v = &downs_v_data;
            uint32_t parse_end;
            if (only_this_scope) parse_end = t->scope_end;
            else                 parse_end = lexer.tokens_ref()[up->tkn_idx].scope_end;

            // go to the correct starting point
            if (only_downs) {
                if (t < t_end && is_bracket_tkn(t))
                    ++t;
                else if (t + 1 < t_end && is_graph_op_tkn(t + 1))
                    t += 2;
            }
            else if (t - 1 > t_begin && ((t - 1)->type == tkn_super_constr_new || (t - 1)->type == tkn_super_constr_append)) {
                t -= 2;
                while (t - 1 > t_begin && (t - 1)->type == '.' && (t - 2)->type == tkn_ident)
                    t -= 2;
            }

            if (t->parsing_flag == Parsing_flag::Currently_parsing) {
                lexer.parsing_error(t, "Detected circle reference.");
                return false;
            }
            else {
                t->parsing_flag = Parsing_flag::Currently_parsing;
            }

            while (t - t_begin <= parse_end) {
#ifdef TRACY_ALL
                ZoneScopedN("parser_token_loop");
                ZoneValue(t->line_pos);
                ZoneValue(t->h_pos);
#endif
                switch (t->type) {

                case tkn_super_constr_new:
                case tkn_super_constr_append:
                case tkn_down_constr_new:
                case tkn_down_constr_append:

                    if (t->type == tkn_super_constr_new || t->type == tkn_super_constr_append) {
                        t_super = t - 1, t_node = t + 1;
                        if (t_super >= t_begin && t < t_end && !(t_super->type == tkn_ident && t_node->type == tkn_ident)) {
                            lexer.parsing_error(t, "Invalid arguments for the super constraint.");
                            return false;
                        }
                    }
                    else if (t->type == tkn_down_constr_new || t->type == tkn_down_constr_append) {
                        t_super = t - 1, t_node = t_super;
                        if (t_node >= t_begin && t < t_end && !(t_node->type == tkn_ident)) {
                            lexer.parsing_error(t, "Invalid arguments for the sub constraint.");
                            return false;
                        }
                    }

                    if (t_node->parsing_flag == Parsing_flag::Finished_parsing) {
                        t = t_begin + t_node->scope_end + 1;
                        continue;
                    }

                    if (up->node_class == Obj_class::Global_space && main_front)
                        t_global_scope_progress = t_begin + t->scope_end;

                    {
                        Obj_class node_class;
                        if (t->type == tkn_super_constr_new)
                            if (((t_node + 1)->type == tkn_down_constr_new || (t_node + 1)->type == tkn_down_constr_append))
                                node_class = Obj_class::Meta_template;
                            else node_class = Obj_class::Base_type;
                        else if (!(up->on_based_branch))
                            node_class = Obj_class::Template;
                        else node_class = Obj_class::None;

                        Ops op = Ops::None;
                        if ((t_node + 2)->type == tkn_array)
                            op = Ops::Array;

                        downs_v->push_back(new Ast_node{ up, uint32_t(t_node - t_begin), node_class, op });
                    }

                    if (!(up->on_based_branch)) {
                        Ast_node* super;
                        if (t->type == tkn_super_constr_new)
                            super = new Ast_node{ up, uint32_t(t_super - t_begin), Obj_class::Base_name };
                        else
                            super = find_super(up, t_super, (t_node + 1)->type != tkn_down_constr_new);

                        if (!super) {
                            if ((t_node + 1)->type == tkn_down_constr_new) {
                                if (up->super->op == Ops::Array)
                                    super = up->super->super;
                                else {
                                    lexer.parsing_error(t_node, "Cannot create new objects inside an object whose super already has subs.");
                                    return false;
                                }
                            }
                        }

                        // go to the first super that is an actual constraint.
                        while (super->super && super->n_downs == 0 && super->node_class != Obj_class::Base_type && super->op != Ops::Array)
                            super = super->super;

                        connect_super(downs_v->back(), super);

                        if (!ast->add_to_ident_lookup(downs_v->back())) {
                            parsing_error(downs_v->back(), "Identifier already declared in this scope.");
                            return false;			    
                        }

                        downs_v->back()->on_based_branch = downs_v->back()->node_class == Obj_class::Base_type || (super->on_based_branch && downs_v->back()->op != Ops::Array);
                    }
                    else {
                        downs_v->back()->on_based_branch = true;
                    }

                    if ((t_super - 1)->type == '~')
                        downs_v->back()->permutable_node = true;

                    if ((t_node + 1)->type == tkn_down_constr_new || (t_node + 1)->type == tkn_down_constr_append) {
                        if (!parse_node_recursive(downs_v->back(), true, 0, false, main_front))
                            return false;
                        t = t_begin + t_node->scope_end + 2;
                    }
                    else
                        t = t_begin + t_node->scope_end + 1;

                    t_node->parsing_flag = Parsing_flag::Finished_parsing;

                    break;
                case tkn_ident:
                    if (!is_graph_op_tkn(t + 1) && !is_binary_op(t + 1))
                        downs_v->push_back(new Ast_node{ up, uint32_t(t - t_begin) });
                    ++t;
                    break;
                case tkn_int:
                case tkn_real:
                case tkn_string:
                    if(!is_binary_op(t + 1))
                        downs_v->push_back(new Ast_node{ up, uint32_t(t - t_begin) });
                    ++t;
                    break;
                case tkn_ghost:
                    downs_v->push_back(new Ast_node{ up, uint32_t(t - t_begin), Obj_class::None, tkn_to_op_table[int(t->type)] });
                    ++t;
                    break;
                case '(':
                case '{':
                case '[':
                case '<':
                case '\'':
                    if (!(up->on_based_branch)) {
                        lexer.parsing_error(t, "Raw brackets are only allowed in expressions or within subs of a type whose super is a base type.");
                    }
                    else {
                        downs_v->push_back(new Ast_node{ up, uint32_t(t - t_begin), Obj_class::None, tkn_to_op_table[int(t->type)] });
                        if (!parse_node_recursive(downs_v->back(), true, 0, false, main_front))
                            return false;
                    }
                    t = t_begin + t->scope_end + 1;
                    break;
                case '%':
                case '&':
                case '+':
                case '-':
                case '*':
                case '/':
                case tkn_eq:
                case tkn_neq:
                case tkn_greater:
                case tkn_less:
                case tkn_less_eq:
                case tkn_greater_eq:
                case tkn_and:
                case tkn_or:
                case tkn_access_down:
                case tkn_access_sub:
                case tkn_access_downs:
                case tkn_access_subs:
                case tkn_concat:
                    downs_v->push_back(parse_expression(downs_v, up, t, { is_binary_op(t) || is_unary_op_left_associative(t) ? uint32_t(t - 1 - t_begin) : uint32_t(t - t_begin),
                        lexer.tokens_ref()[up->tkn_idx].scope_end }, true));
                    if (!downs_v->back()) {
                        lexer.parsing_error(t - 1, "Failed to parse this expression.");
                        return false;
                    }
                    {
                        std::variant<bool, int64_t, double> const_math_result = eval_const_math(downs_v->back());
                        if (const_math_result.index() != 0) {
                            if (const_math_result.index() == 1)
                                lexer.add_token_post_lex(tkn_int, &std::get<1>(const_math_result));
                            else if (const_math_result.index() == 2)
                                lexer.add_token_post_lex(tkn_real, &std::get<2>(const_math_result));
                            downs_v->back()->~Ast_node();
                            downs_v->pop_back();
                            downs_v->push_back(new Ast_node{ up, uint32_t(lexer.tokens_ref().end() - 1 - t_begin) });
                        }
                    }
                    break;
                default:
                    ++t;
                    continue;
                }

                if (up->node_class == Obj_class::Global_space && main_front)
                    t_global_scope_progress = t_begin + t->scope_end;
            }

            if (!offset)
                (t_begin + up->tkn_idx)->parsing_flag = Parsing_flag::Finished_parsing;

            Ast_node** prev_downs = up->downs;
            uint32_t prev_n_downs = up->n_downs;

            up->n_downs += (uint16_t)downs_v->size();
            up->downs = new Ast_node * [up->n_downs];

            for (uint32_t i = 0; i < prev_n_downs; ++i)
                up->downs[i] = prev_downs[i];
            for (uint32_t i = prev_n_downs; i < up->n_downs; ++i)
                up->downs[i] = (*downs_v)[i - prev_n_downs];

            if (prev_downs)
                delete[] prev_downs;

            return true;
        }

        bool is_graph_op_tkn(std::vector<Token>::iterator t) const {
            return t->type == tkn_super_constr_new ||
                t->type == tkn_super_constr_append ||
                t->type == tkn_down_constr_new ||
                t->type == tkn_down_constr_append ||
                t->type == tkn_access_downs ||
                t->type == tkn_access_subs;
        }

        bool is_declaration_ident(const std::vector<Token>::iterator t) {
            if (t->type == tkn_ident &&
                ((t + 1)->type == tkn_down_constr_new || (t + 1)->type == tkn_down_constr_append ||
                    (t - 1)->type == tkn_super_constr_new || (t - 1)->type == tkn_super_constr_append))
                return true;
            return false;
        }

        bool is_bracket_tkn(const std::vector<Token>::iterator t) const {
            return t->type == '(' || t->type == '{' || t->type == '[' || t->type == '<' || t->type == '\'';
        }

        bool is_opening_bracket_tkn(const std::vector<Token>::iterator t) const {
            return t->type == '(' || t->type == '{' || t->type == '[' || t->type == '<';
        }

        bool is_closing_bracket_tkn(const std::vector<Token>::iterator t) const {
            return t->type == ')' || t->type == '}' || t->type == ']' || t->type == '>';
        }

        bool is_arithmetic_op(const std::vector<Token>::iterator t) const {
            return (t->type >= tkn_eq && t->type <= tkn_or) || t->type == '+' || t->type == '-' || t->type == '*' || t->type == '/';
        }

        bool is_arithmetic_tkn_left(const std::vector<Token>::iterator t) const {
            return t->type == tkn_int || t->type == tkn_real || t->type == tkn_ident || is_closing_bracket_tkn(t);
        }

        bool is_arithmetic_tkn_right(const std::vector<Token>::iterator t) const {
            return t->type == tkn_int || t->type == tkn_real || t->type == tkn_ident || is_opening_bracket_tkn(t);
        }

        bool is_graph_access_single_op(const std::vector<Token>::iterator t) const {
            return t->type == tkn_access_down || t->type == tkn_access_sub;
        }

        bool is_graph_access_all_op(const std::vector<Token>::iterator t) const {
            return t->type == tkn_access_downs || t->type == tkn_access_subs;
        }

        bool is_op(const std::vector<Token>::iterator t) const {
            return is_arithmetic_op(t) || is_graph_access_single_op(t) || is_graph_access_all_op(t) || t->type == '&' || t->type == '%' || t->type == tkn_concat;
        }

        bool is_binary_op(const std::vector<Token>::iterator t) const {
            return (is_arithmetic_op(t) || is_graph_access_single_op(t) || t->type == tkn_concat)
                && (is_arithmetic_tkn_left(t - 1) || is_op(t - 1)) && (is_arithmetic_tkn_right(t + 1) || is_op(t + 1));
        }

        bool is_unary_op_right_associative(const std::vector<Token>::iterator t) const {
            return (t->type == '&' || t->type == '%') && !is_arithmetic_tkn_left(t - 1) && is_arithmetic_tkn_right(t + 1); // t->type == '+' || t->type == '-'
        }

        bool is_unary_op_left_associative(const std::vector<Token>::iterator t) const {
            return is_graph_access_all_op(t) && is_arithmetic_tkn_left(t - 1) && !is_arithmetic_tkn_right(t + 1);
        }
    };
}
