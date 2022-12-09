#include <fmt/format.h>
#include <clopts.hh>
#include <compiler.hh>

/// ===========================================================================
///  Command-line options.
/// ===========================================================================
using namespace command_line_options;
using options = clopts<// clang-format off
    positional<"file", "The file to compile", file_data>,
    option<"-o", "The output file">,
    flag<"--print-ast", "Print the AST and exit">,
    help
>; // clang-format on

/// ===========================================================================
///
/// ===========================================================================

int main(int argc, char** argv) {
    options::parse(argc, argv);
    ebnfc::parser p{options::get<"file">()->contents, options::get<"file">()->path.string()};
    auto tree = p.parse();

    if (options::get<"--print-ast">()) fmt::print(stdout, "{}", ebnfc::print_tree(tree.get()));
    else {
        ebnfc::emit_options opts;
        opts.parser_base_class = "lexer";
        opts.parser_namespace = "ebnfc";
        opts.parser_name = "parser";
        opts.parser_base_initialiser = "lexer{input, path}";

        std::string code;
        ebnfc::emit(tree.get(), code, opts);
        fmt::print(stdout, "{}", code);
    }
}