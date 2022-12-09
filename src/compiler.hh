#ifndef EBNFC_COMPILER_HH
#define EBNFC_COMPILER_HH

#include <algorithm>
#include <functional>
#include <mutex>
#include <ranges>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <utility>
#include <utils.hh>
#include <variant>

namespace ebnfc {
using namespace std::literals;
namespace ranges = std::ranges;

struct parse_error : std::runtime_error {
    template <typename... arguments>
    parse_error(fmt::format_string<arguments...> fmt, arguments&&... args)
        : std::runtime_error(fmt::format(fmt, std::forward<arguments>(args)...)) {}
};

/// ===========================================================================
///  Context
/// ===========================================================================
struct context {
};

/// ===========================================================================
///  Tokens
/// ===========================================================================
enum struct tk : u8 {
    invalid,
    eof,
    code,
    nonterminal,
    def,
    semicolon,
    alternative,
    arrow,
    identifier,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbrack,
    rbrack,
};

constexpr std::string_view tk_to_str(tk t) {
    switch (t) {
        case tk::invalid: return "invalid"sv;
        case tk::eof: return "eof"sv;
        case tk::code: return "code"sv;
        case tk::nonterminal: return "nonterminal"sv;
        case tk::def: return "def"sv;
        case tk::semicolon: return "semicolon"sv;
        case tk::alternative: return "alternative"sv;
        case tk::arrow: return "arrow"sv;
        case tk::identifier: return "identifier"sv;
        case tk::lparen: return "lparen"sv;
        case tk::rparen: return "rparen"sv;
        case tk::lbrace: return "lbrace"sv;
        case tk::rbrace: return "rbrace"sv;
        case tk::lbrack: return "lbrack"sv;
        case tk::rbrack: return "rbrack"sv;
        default: return "???";
    }
}

struct loc {
    u32 start;
    u32 end;

    loc() : start(0), end(0) {}
    loc(std::integral auto _start, std::integral auto _end)
        : start(u32(_start)),
          end(u32(_end)) {}
};

struct token {
    tk type = tk::invalid;
    std::string text;
    std::vector<std::string> strings;
    u64 integer{};
    loc pos;
};

/// ===========================================================================
///  Diagnostics.
/// ===========================================================================
enum struct severity {
    error,
    warning,
    note,
    ice,
};

constexpr std::string_view severity_colour(severity s) {
    switch (s) {
        case severity::error: return "\033[31m";
        case severity::warning: return "\033[33m";
        case severity::note: return "\033[32m";
        case severity::ice: return "\033[35m";
    }
    return "\x1b[0m";
}

constexpr std::string_view severity_name(severity s) {
    switch (s) {
        case severity::error: return "Error";
        case severity::warning: return "Warning";
        case severity::note: return "Note";
        case severity::ice: return "Internal Compiler Error";
    }
    return "UNKNOWN SEVERITY";
}

struct diag {
    std::string message;
    std::string_view input;
    std::string_view filename;
    severity sev;
    mutable loc pos;

    [[nodiscard]] std::string str() const {
        if (input.empty()) {
            return fmt::format("{}:??:??: {}\033[1m{}:\033[m\033[1;38m {}\033[m\n", //
                               filename, severity_colour(sev), severity_name(sev), message);
        }

        if (pos.start > input.size()) pos.start = u32(input.size());
        if (pos.end > input.size()) pos.end = u32(input.size());

        /// Seek to the start of the line. Keep track of the line number.
        u32 line = 1;
        u32 line_start = 0;
        for (u32 i = pos.start; i > 0; --i) {
            if (input[i] == '\n') {
                if (not line_start) line_start = i + 1;
                ++line;
            }
        }

        /// Don’t include the newline in the line.
        if (input[line_start] == '\n') ++line_start;

        /// Seek to the end of the line.
        u32 line_end = pos.end;
        while (line_end < input.size() && input[line_end] != '\n') line_end++;

        /// Print the filename, line and column, severity and message.
        std::string result;
        result += fmt::format("{}:{}:{}: {}\033[1m{}:\033[m\033[1;38m {}\033[m\n", //
                              filename, line, pos.start - line_start,              //
                              severity_colour(sev), severity_name(sev), message);

        /// Print the line.
        result += fmt::format(" {} | ", line);
        for (u32 i = line_start; i < pos.start; ++i) {
            if (input[i] == '\t') result += "    ";
            else result += input[i];
        }
        result += fmt::format("\033[1m{}", severity_colour(sev));
        for (u32 i = pos.start; i < pos.end; ++i) {
            if (input[i] == '\t') result += "    ";
            else result += input[i];
        }
        result += fmt::format("\033[m");
        for (u32 i = pos.end; i < line_end; ++i) {
            if (input[i] == '\t') result += "    ";
            else result += input[i];
        }
        result += "\n";

        /// Underline the region with a caret.
        result += fmt::format("{}  | \033[1m{}",                                          //
                              std::string(not line ? 1 : u32(std::log10(line) + 1), ' '), //
                              severity_colour(sev));
        for (u32 i = line_start; i < pos.start; ++i) {
            if (input[i] == '\t') result += "    ";
            else result += ' ';
        }
        for (u32 i = pos.start; i < pos.end; ++i) {
            if (input[i] == '\t') result += "~~~~";
            else result += '~';
        }
        result += fmt::format("\033[0m\n");
        return result;
    }
};

/// ===========================================================================
///  Lexer.
/// ===========================================================================
/// Check if a character is allowed at the start of an identifier.
constexpr bool isstart(char c) { return std::isalpha(c) or c == '_' or c == '-'; }

/// Check if a character is allowed in an identifier.
constexpr bool iscontinue(char c) { return std::isalnum(c) or c == '_' or c == '-'; }

/// Check if a character is a delimiter.
constexpr bool isdelim(char c) {
    return std::isspace(c) or c == '(' or c == ')' or c == '{' or c == '}' //
           or c == '[' or c == ']' or c == ';' or c == '|'                 //
           or c == ':' or c == '<' or c == '>';
}

/// Main lexer context.
struct lexer {
    std::string input;
    std::string filename;
    const char* curr;
    const char* end;
    char lastc;
    token tok;

    /// Handle a diagnostic. If the severity is error or ice, then this
    /// function MUST not return. Instead, it may raise and exception or
    /// terminate the program.
    std::function<void(diag&&)> diagnostic_handler = [](diag&& d) {
        fmt::print(stderr, "{}", d.str());
        if (d.sev == severity::error or d.sev == severity::ice) std::exit(1);
    };

    explicit lexer() = default;
    explicit lexer(std::string_view _input, std::string_view _filename = "<input>")
        : input{_input},
          filename{_filename},
          curr{input.data()},
          end{input.data() + input.size()},
          lastc{' '} {
        next_char();
        next();
    }

    /// Issue a diagnostic.
    template <typename... arguments>
    void issue_diagnostic(severity sev, loc pos, fmt::format_string<arguments...> fmt, arguments&&... args) {
        diagnostic_handler(diag{
            fmt::format(fmt, std::forward<arguments>(args)...),
            input,
            filename,
            sev,
            pos});
        if (sev == severity::ice) std::exit(1);
    }

    template <typename... arguments>
    [[noreturn]] void err(loc pos, fmt::format_string<arguments...> fmt, arguments&&... args) {
        issue_diagnostic(severity::error, pos, fmt, std::forward<arguments>(args)...);
        issue_diagnostic(severity::ice, pos, "Diagnostic handler returned after handling an error");
        __builtin_unreachable();
    }

    template <typename... arguments>
    void warn(loc pos, fmt::format_string<arguments...> fmt, arguments&&... args) {
        issue_diagnostic(severity::warning, pos, fmt, std::forward<arguments>(args)...);
    }

    template <typename... arguments>
    void note(loc pos, fmt::format_string<arguments...> fmt, arguments&&... args) {
        issue_diagnostic(severity::note, pos, fmt, std::forward<arguments>(args)...);
    }

    u32 here() { return u32(curr - input.data()); }

    void next_char() {
        /// Keep returning EOF once EOF has been reached.
        if (curr >= end) {
            lastc = 0;
            return;
        }

        lastc = *curr++;
        if (lastc == '\r') lastc = '\n';
    }

    void next_ident() {
        tok.text.clear();
        do {
            if (lastc == '-') tok.text += '_';
            else tok.text += lastc;
            next_char();
        } while (iscontinue(lastc));

        tok.pos.end = here();
        if (not std::isspace(lastc) and lastc != 0 and not isdelim(lastc)) err(tok.pos, "Invalid character in identifier");
    }

    void next() {
        /// If we’re at end of file, return EOF.
        if (curr >= end) {
            tok.type = tk::eof;
            tok.pos.start = tok.pos.end = u32(input.size());
            return;
        }

        /// Skip whitespace.
        skip_whitespace();

        /// Set the start of the token.
        tok.pos.start = u32(curr - input.data() - 1);
        tok.type = tk::invalid;

        /// Lex the token.
        switch (lastc) {
            case 0: tok.type = tk::eof; break;
            case '(':
                tok.type = tk::lparen;
                next_char();
                break;
            case ')':
                tok.type = tk::rparen;
                next_char();
                break;
            case '[':
                tok.type = tk::lbrack;
                next_char();
                break;
            case ']':
                tok.type = tk::rbrack;
                next_char();
                break;
            case '{':
                tok.type = tk::lbrace;
                next_char();
                break;
            case '}':
                tok.type = tk::rbrace;
                next_char();
                break;
            case ';':
                tok.type = tk::semicolon;
                next_char();
                break;
            case '|':
                tok.type = tk::alternative;
                next_char();
                break;
            case '<':
                next_char();
                tok.type = tk::nonterminal;
                next_ident();
                if (lastc != '>') err(tok.pos, "Expected '>' at end of nonterminal");
                next_char();
                break;
            case ':':
                next_char();
                if (lastc != ':') err(tok.pos, "Expected `::=`");
                next_char();
                if (lastc != '=') err(tok.pos, "Expected `::=`");
                next_char();
                tok.type = tk::def;
                break;

            default: {
                /// Underscores in identifiers are allowed, as well as hyphens.
                if (isstart(lastc)) {
                    tok.type = tk::identifier;
                    return next_ident();
                }

                /// Error.
                else
                    err({here() - 1, here()}, "invalid character");
            }
        }

        /// Set the end of the token if we didn’t already.
        tok.pos.end = u32(curr - input.data() - 1);
    }

    void dump() {
        do {
            note(tok.pos, "Token: {}", tk_to_str(tok.type));
            if (tok.type == tk::invalid) std::exit(42);
            next();
        } while (tok.type != tk::eof);
    }

    void skip_whitespace() {
        while (std::isspace(lastc)) next_char();
    }
};

/// ===========================================================================
///  AST
/// ===========================================================================
/// Base for a node in the AST.
struct tree_node {
    loc pos;
    tree_node* parent{};
    virtual ~tree_node() = default;
};

/// Tree node.
using tree = std::unique_ptr<tree_node>;

/// An AST node containing child nodes.
struct tree_node_container : tree_node {
    tree_node_container() = default;
    std::vector<tree> children;

    void add(tree&& t) {
        t->parent = this;
        children.push_back(std::move(t));
    }
};

/// The root of the AST.
struct tree_node_root final : tree_node_container {
    std::string code_before;
    std::string code_after;
};

/// The root may contain code.
struct tree_node_code final : tree_node {
    std::string text;
};

/// A rule in the AST.
struct tree_node_rule final : tree_node_container {
    std::string nonterminal;
};

/// An alternative of a rule.
struct tree_node_alternative final : tree_node_container {
    std::string return_type;
    std::string code;
};

/// A term in an alternative.
struct tree_node_nonterminal final : tree_node {
    std::string text;
};
struct tree_node_identifier final : tree_node {
    std::string text;
};
struct tree_node_group final : tree_node_container {};
struct tree_node_optional final : tree_node_container {};
struct tree_node_repetition final : tree_node_container {};

/// ===========================================================================
///  Parser
/// ===========================================================================
struct parser : lexer {
    /// Forward input and filename to the lexer.
    parser(std::string_view code, std::string_view path) : lexer(code, path) {}

    /// Make a new AST node.
    template <typename node>
    requires requires { static_cast<node*>(std::declval<tree_node*>()); }
    auto make() -> std::pair<node*, tree> {
        auto t = new node{};
        return {t, tree{static_cast<tree_node*>(t)}};
    }

    /// <grammar> ::= <rule> | CODE
    tree parse() {
        auto [root, t] = make<tree_node_root>();
        bool rule_seen = false;

        /// Parse the rules and top-level code.
        while (tok.type != tk::eof) {
            /// Add code to the root.
            if (tok.type == tk::code) {
                if (not rule_seen) root->code_before += tok.text;
                else root->code_after += tok.text;
            }

            /// Parse a rule and add it.
            else {
                rule_seen = true;
                root->add(parse_rule());
            }
        }

        /// Need at least one rule.
        if (not rule_seen) err(root->pos, "Expected at least one rule");
        return std::move(t);
    }

    /// <rule> ::= NONTERMINAL ASSIGN <alternatives> [ SEMICOLON ]
    tree parse_rule() {
        /// Get the name of the rule.
        auto [rule, t] = make<tree_node_rule>();
        if (tok.type != tk::nonterminal) err(tok.pos, "Expected nonterminal");
        rule->nonterminal = std::move(tok.text);
        rule->pos = tok.pos;
        tok.text = {};
        next();

        /// Yeet "::=".
        if (tok.type != tk::def) err(tok.pos, "Expected `::=`");
        next();

        /// Parse the alternatives.
        parse_alternatives(rule->children);
        if (tok.type == tk::semicolon) next();
        return std::move(t);
    }

    /// <alternatives> ::= <alternative> { ALTERNATIVE <alternative> }
    void parse_alternatives(std::vector<tree>& nodes) {
        /// Parse the first alternative.
        nodes.push_back(parse_alternative());

        /// Parse the rest of the alternatives.
        while (tok.type == tk::alternative) {
            next();
            nodes.push_back(parse_alternative());
        }
    }

    /// <alternative> ::= <term> { <term> } [ ARROW IDENTIFIER ] [ CODE ]
    tree parse_alternative() {
        auto [alt, t] = make<tree_node_alternative>();

        /// Parse the first term.
        alt->add(parse_term());
        alt->pos = alt->children[0]->pos;

        /// Parse the rest of the terms.
        ///
        /// If the current token is a nonterminal, we skip whitespace
        /// in the lexer and see if we end up with a colon; if we do,
        /// then it’s likely that the next token is '::=', which means
        /// that this is the start of a new rule.
        while ((tok.type == tk::nonterminal and (skip_whitespace(), lastc != ':'))
               or tok.type == tk::identifier
               or tok.type == tk::lparen
               or tok.type == tk::lbrack
               or tok.type == tk::lbrace) {
            alt->add(parse_term());
        }

        /// Parse the return type if there is one.
        if (tok.type == tk::arrow) {
            next();
            if (tok.type != tk::identifier) err(tok.pos, "Expected identifier");
            alt->return_type = std::move(tok.text);
            tok.text = {};
            next();
        }

        /// Add the code if there is any.
        if (tok.type == tk::code) {
            alt->code = std::move(tok.text);
            tok.text = {};
            next();
        }

        return std::move(t);
    }

    /// <term> ::= NONTERMINAL | IDENTIFIER | <group> | <optional> | <repetition>
    tree parse_term() {
        switch (tok.type) {
            case tk::nonterminal: {
                auto [nt, t] = make<tree_node_nonterminal>();
                nt->text = std::move(tok.text);
                nt->pos = tok.pos;
                tok.text = {};
                next();
                return std::move(t);
            }
            case tk::identifier: {
                auto [id, t] = make<tree_node_identifier>();
                id->text = std::move(tok.text);
                id->pos = tok.pos;
                tok.text = {};
                next();
                return std::move(t);
            }
            case tk::lparen: return parse_bracketed_alternatives<tree_node_group, tk::rparen>();
            case tk::lbrack: return parse_bracketed_alternatives<tree_node_optional, tk::rbrack>();
            case tk::lbrace: return parse_bracketed_alternatives<tree_node_repetition, tk::rbrace>();
            default: err(tok.pos, "Expected term");
        }
    }

    /// <group>      ::= LPAREN <alternatives> RPAREN
    /// <optional>   ::= LBRACK <alternatives> RBRACK
    /// <repetition> ::= LBRACE <alternatives> RBRACE
    template <typename node_type, tk close>
    tree parse_bracketed_alternatives() {
        auto [node, t] = make<node_type>();
        node->pos = tok.pos;
        next();
        parse_alternatives(node->children);
        if (tok.type != close) err(tok.pos, "Expected '{}'", tk_to_str(close));
        next();
        return std::move(t);
    }
};

/// ===========================================================================
///  Print the AST.
/// ===========================================================================
/// Print a rule as BNF.
std::string print_as_bnf(const tree_node* node) {
    std::string s;

    /// Rule.
    if (auto rule = dynamic_cast<const tree_node_rule*>(node)) {
        /// Print the rule name.
        s += fmt::format("<{}> ::= ", rule->nonterminal);

        /// Print the alternatives.
        for (auto it = rule->children.begin(); it != rule->children.end(); ++it) {
            if (it != rule->children.begin()) s += fmt::format(" | ");
            s += print_as_bnf(it->get());
        }
    }

    /// Alternatives.
    else if (auto alt = dynamic_cast<const tree_node_alternative*>(node)) {
        /// Print the terms.
        for (auto it = alt->children.begin(); it != alt->children.end(); ++it) {
            if (it != alt->children.begin()) s += fmt::format(" ");
            s += print_as_bnf(it->get());
        }
    }

    /// Nonterminal.
    else if (auto nt = dynamic_cast<const tree_node_nonterminal*>(node)) {
        s += fmt::format("<{}>", nt->text);
    }

    /// Identifier.
    else if (auto id = dynamic_cast<const tree_node_identifier*>(node)) {
        s += fmt::format("{}", id->text);
    }

    /// Group.
    else if (auto group = dynamic_cast<const tree_node_group*>(node)) {
        s += fmt::format("( ");
        s += print_as_bnf(group->children[0].get());
        s += fmt::format(" )");
    }

    /// Optional.
    else if (auto opt = dynamic_cast<const tree_node_optional*>(node)) {
        s += fmt::format("[ ");
        s += print_as_bnf(opt->children[0].get());
        s += fmt::format(" ]");
    }

    /// Repetition.
    else if (auto rep = dynamic_cast<const tree_node_repetition*>(node)) {
        s += fmt::format("{{ ");
        s += print_as_bnf(rep->children[0].get());
        s += fmt::format(" }}");
    }

    /// Don’t print anything else.
    else {}

    /// Done.
    return s;
}

/// Print a parse tree.
std::string print_tree(const tree_node* t, const std::string& leading_text = "") {
    static const auto dump_children = [](const std::vector<tree>& children, std::string text) -> std::string {
        std::string s;
        for (auto it = children.begin(); it != children.end(); ++it) {
            s += fmt::format("\033[31m{}{}", text, it + 1 == children.end() ? "└─" : "├─");
            s += print_tree(it->get(), text + (it + 1 == children.end() ? "  " : "│ "));
        }
        return s;
    };

    /// Return string.
    std::string s;

    /// Root node.
    if (auto* root = dynamic_cast<const tree_node_root*>(t)) {
        for (auto it = root->children.begin(); it != root->children.end(); ++it) {
            if (it != root->children.begin()) s += fmt::format("\n");
            s += print_tree(it->get());
        }
    }

    /// Rule node.
    else if (auto* rule = dynamic_cast<const tree_node_rule*>(t)) {
        s += fmt::format("\033[37m/// ");
        s += print_as_bnf(rule);
        s += fmt::format("\n\033[31mRule \033[35m<{}>\n", t->pos.start);
        s += dump_children(rule->children, leading_text);
    }

    /// Alternative node.
    else if (auto* alt = dynamic_cast<const tree_node_alternative*>(t)) {
        s += fmt::format("\033[31mAlternative \033[35m<{}>\n", t->pos.start);
        s += dump_children(alt->children, leading_text);
    }

    /// Nonterminal node.
    else if (auto* nt = dynamic_cast<const tree_node_nonterminal*>(t)) {
        s += fmt::format("\033[31mNonterminal \033[35m<{}> \033[32m<{}>\033[0m\n", t->pos.start, nt->text);
    }

    /// Identifier node.
    else if (auto* id = dynamic_cast<const tree_node_identifier*>(t)) {
        s += fmt::format("\033[31mIdentifier \033[35m<{}> \033[33m{}\033[0m\n", t->pos.start, id->text);
    }

    /// Group node.
    else if (auto* group = dynamic_cast<const tree_node_group*>(t)) {
        s += fmt::format("\033[31mGroup \033[35m<{}>\n", t->pos.start);
        s += dump_children(group->children, leading_text);
    }

    /// Optional node.
    else if (auto* opt = dynamic_cast<const tree_node_optional*>(t)) {
        s += fmt::format("\033[31mOptional \033[35m<{}>\n", t->pos.start);
        s += dump_children(opt->children, leading_text);
    }

    /// Repetition node.
    else if (auto* rep = dynamic_cast<const tree_node_repetition*>(t)) {
        s += fmt::format("\033[31mRepetition \033[35m<{}>\n", t->pos.start);
        s += dump_children(rep->children, leading_text);
    }

    /// Unknown node.
    else { throw std::runtime_error("Unknown node type"); }

    /// Done.
    return s;
}

/// ===========================================================================
///  Emit code.
/// ===========================================================================
struct emit_options {
    std::string parser_namespace = "ff";
    std::string parser_name = "parser";
    std::string parser_base_class;
    std::string parser_base_initialiser;
};

/// Emit C++ code that implements a recursive-descent parser that parses a tree.
void emit(const tree_node* t, std::string& out, emit_options& opts) {
    /// Emit the root.
    if (const auto* root = dynamic_cast<const tree_node_root*>(t)) {
        /// This is a good place to emit the parser skeleton.
        fmt::format_to(std::back_inserter(out), R"c++(/// =========================================================================== ///
///                                                                             ///
///            This file was generated from <FILENAME> using EBNFC              ///
///                                                                             ///
/// =========================================================================== ///

/// Include the parser skeleton.
#define EBNFC_NAMESPACE_NAME {0}
#include <ebnfc/skeleton.hh>
#undef EBNFC_NAMESPACE_NAME

/// Parser namespace.
namespace {1} {{

/// Code specified before any rules.
{2}

/// Main parser context.
struct {3} {4}{5} {{
    /// Create a new parser.
    parser(std::string_view code, std::string_view path) {6}{7} {{}}

    /// Read the next token.
    void next();

    /// Make a new AST node.
    template <typename node>
    requires requires {{ static_cast<node*>(std::declval<tree_node*>()); }}
    auto make() -> std::pair<node*, tree> {{
        auto t = new node{{}};
        return {{t, tree{{static_cast<tree_node*>(t)}}}};
    }}

)c++",
                       opts.parser_namespace,                            /// 0
                       opts.parser_namespace,                            /// 1
                       root->code_before,                                /// 2
                       opts.parser_name,                                 /// 3
                       opts.parser_base_class.empty() ? "" : ": ",       /// 4
                       opts.parser_base_class,                           /// 5
                       opts.parser_base_initialiser.empty() ? "" : ": ", /// 6
                       opts.parser_base_initialiser);                    /// 7

        for (auto& child : root->children) emit(child.get(), out, opts);

        /// Emit the rest of the parser skeleton.
        fmt::format_to(std::back_inserter(out), R"c++(}}

/// Code specified after any rules.
{}

}} // namespace {}
)c++",
                       root->code_after, opts.parser_namespace);
    }

    /// Emit a function to parse the rule.
    else if (const auto* rule = dynamic_cast<const tree_node_rule*>(t)) {
        /// Emit the rule as a comment.
        out += "    /// ";
        out += print_as_bnf(rule);
        out += "\n";

        /// Print the start of the function.
        fmt::format_to(std::back_inserter(out), "    auto parse_{}() -> tree {{\n"
                                                "        tree $0;\n\n",
                       rule->nonterminal);

        /// Emit each alternative.
        /// TODO: Handle left-recursion.
        for (const auto& alt : rule->children) emit(alt.get(), out, opts);

        /// Emit the end of the function.
        fmt::format_to(std::back_inserter(out), "        /// If no alternative matches, return an error.\n"
                                                "        else error(here(), \"Unexpected token\");\n"
                                                "        return $0;\n"
                                                "    }}\n\n");
    }

    /// Emit an alternative.
    else if (const auto* alt = dynamic_cast<const tree_node_alternative*>(t)) {
        /// If the first term of the alternative is a nonterminal, call the
        /// function that parses that nonterminal; otherwise, match the token.
        /// TODO.
    }

    /// Unknown node.
    else {
        die("Unknown node type");
    }
}

} // namespace ebnfc

#endif // EBNFC_COMPILER_HH
