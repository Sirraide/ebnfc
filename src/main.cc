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
    help
>; // clang-format on

/// ===========================================================================
///
/// ===========================================================================

int main(int argc, char** argv) {
    options::parse(argc, argv);
    ebnfc::lexer l{options::get<"file">()->contents, options::get<"file">()->path.string()};
    l.dump();
}