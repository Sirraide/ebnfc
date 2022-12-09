#ifndef EBNFC_COMPILER_HH
#define EBNFC_COMPILER_HH

#include <algorithm>
#include <functional>
#include <mutex>
#include <ranges>
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

    lexer() = default;
    lexer(std::string_view _input, std::string_view _filename = "<input>")
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
        while (std::isspace(lastc)) next_char();

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
                tok.type = tk::def;
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
};

/// ===========================================================================
///  AST
/// ===========================================================================
/// Base for a node in the AST.
struct tree_node {
    loc pos;
    tree_node* parent;
    virtual ~tree_node() {}
};

/// Tree node.
using tree = std::unique_ptr<tree_node>;

/// An AST node containing child nodes.
struct tree_node_container : public tree_node {
    tree_node_container() = default;
    std::vector<tree> children;

    void add(tree&& t) {
        t->parent = this;
        children.push_back(std::move(t));
    }
};

/// The root of the AST.
struct tree_node_root : tree_node_container {};

/// The root may contain code.
struct tree_node_code : tree_node {
    std::string text;
};

/// A rule in the AST.
struct tree_node_rule : public tree_node_container {
    std::string nonterminal;
};

/// An alternative of a rule.
struct tree_node_alternative : public tree_node_container {
    std::string return_type;
    std::string code;
};

/// A term in an alternative.
struct tree_node_nonterminal : tree_node {
    std::string text;
};
struct tree_node_identifier : tree_node {
    std::string text;
};
struct tree_node_group : tree_node_container {};
struct tree_node_optional : tree_node_container {};
struct tree_node_repetition : tree_node_container {};

/// ===========================================================================
///  Parser
/// ===========================================================================
struct parser : lexer {
    /// Forward input and filename to the lexer.
    parser(std::string input, std::string filename) : lexer(std::move(input), std::move(filename)) {}

    /// Make a new AST node.
    template <typename node>
    requires requires { static_cast<node*>(std::declval<tree_node*>()); }
    auto make() -> std::pair<node*, tree> {
        auto t = new node{};
        return {t, tree{static_cast<tree_node*>(t)}};
    }

    /// <grammar> ::= <rule> | CODE
    tree parse_grammar() {
        auto [root, t] = make<tree_node_root>();
        while (tok.type != tk::eof) {
            /// Add code as a node to the root.
            if (tok.type == tk::code) {
                auto [code, node] = make<tree_node_code>();
                code->text = std::move(tok.text);
                tok.text = {};
                root->add(std::move(node));
                next();
            }

            /// Parse a rule and add it.
            else { root->add(parse_rule()); }
        }
        return std::move(t);
    }
};

} // namespace ebnfc

#endif // EBNFC_COMPILER_HH
