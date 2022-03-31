# Minblur Core
This is the core of the Minblur compiler. It's the compiler, built-in macros, etc.

# Browsing the Code
The main "compiler" object is `compiler::compiler_env::CompilerEnv`, it serves as the API
for using the compiler. The `parser::statement::StatementData` enum defines the different
types of statements the compiler deals with.

Parsing uses the `nom` library to build a set of tokens (see `parser::token`) then converts
those tokens into `Statement`s.
