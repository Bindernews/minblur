mod compiler_env;
pub mod consts;
pub mod error;
pub mod format;
pub mod instruction;
pub mod macros;

#[cfg(any(target_family = "windows", target_family = "unix"))]
pub use compiler_env::OsSystemApi;
pub use compiler_env::{CompilerEnv, EnvMode, GenerateCode, SystemApi};
pub use error::{CompileError, PassError};

#[cfg(test)]
mod test {
    use super::*;
    /// trim-indent amount for the current module; change if indentation changes
    const MOD_INDENT: usize = 8;

    fn new_compiler() -> CompilerEnv {
        let mut env = CompilerEnv::new(OsSystemApi::new());
        env.register_builtin_macros().unwrap();
        env
    }

    fn trim_indent(s: &str, indent: usize) -> String {
        let prefix = " ".repeat(indent);
        let mut out = String::new();
        for line in s.lines() {
            out.push_str(line.strip_prefix(&prefix).unwrap_or(line));
            out.push('\n');
        }
        out
    }

    /// Tries to compile the code and assert that the output matches what is expected
    fn compile_code(code: &str) -> String {
        let code = trim_indent(code, MOD_INDENT);
        let mut comp = new_compiler();
        comp.compile_and_generate("test", &code).unwrap()
    }

    /// Tries to compile the code and assert that the resulting error matches the expected string
    fn compile_fmt_error(code: &str) -> String {
        let code = trim_indent(code, MOD_INDENT);
        let mut comp = new_compiler();
        format!(
            "{:?}",
            comp.compile_and_generate("test", &code).unwrap_err()
        )
    }

    /// Tries to compile the code and assert that the resulting error matches the expected string
    fn compile_error_display(code: &str) -> String {
        let code = trim_indent(code, MOD_INDENT);
        let mut comp = new_compiler();
        format!("{}", comp.compile_and_generate("test", &code).unwrap_err())
    }

    #[test]
    fn test_all_instructions() {
        // TODO test more instructions
        let code = r#"
        .macro t_op(sym1, sym2, sym3, sym4)
          op $sym1 a 1 1
          op $sym2 b 2 2
          op $sym3 c 3 3
          op $sym4 d 4 4
        .endmacro
        read result cell1 0
        write a cell1 0
        draw clear 0 0 0 0 0 0
        draw color 255 20 20 255 0 0
        draw stroke 1 0 0 255 0 0
        draw line 10 10 20 20 0 0
        draw rect 10 10 20 20 0 0
        draw lineRect 10 10 20 20 0 0
        draw poly 10 10 20 20 0 0
        draw linePoly 10 10 20 20 0 0
        draw triangle 10 10 20 20 0 0
        draw image 10 10 @copper 32 0 0
        print "frog"
        print a
        print 6
        drawflush display1
        printflush message1
        getlink result 0
        control enabled block1 0 0 0 0
        control shoot block1 x y 1 0
        control shootp block1 u 1 0 0
        control configure block1 c 1 0 0
        control color block1 1 1 1 0
        radar enemy any any distance turret1 1 result
        sensor result block1 @copper
        set result 0
        t_op!(add, sub, mul, div)
        t_op!(idiv, mod, pow, land)
        t_op!(equal, notEqual, lessThan, lessThanEq)
        t_op!(greaterThan, greaterThanEq, strictEqual, or)
        t_op!(shl, shr, xor, flip)
        t_op!(max, min, angle, len)
        # TODO rest of op variants
        jump -1 notEqual x false
        ubind @poly
        ucontrol idle 0 0 0 0 0
        ucontrol stop 0 0 0 0 0
        ucontrol move 0 0 0 0 0
        ucontrol approach 0 0 0 0 0
        ucontrol boost 0 0 0 0 0
        ucontrol pathfind 0 0 0 0 0
        # TODO rest of ucontrol variants
        uradar enemy any any distance 0 1 result
        # TODO rest of uradar variants
        ulocate building core true @copper outx outy found building
        # TODO rest of ulocate variants
        end
        "#;
        let code = trim_indent(code, MOD_INDENT);
        let mut comp = new_compiler();
        insta::assert_display_snapshot!(comp.compile_and_generate("test", &code).unwrap());
    }

    #[test]
    fn test_bad_math() {
        let code = r"
        m! r1 = 7 * 3 &&& 5
        m! r2 = ++++++
        ";
        assert_eq!(
            compile_error_display(code),
            "Error in test at 2:1\n| at 1:14 near '&' => empty token group\n\nError in test at 3:1\n| at 1:6 near '+' => unexpected operator\n\n"
        );
    }

    /// Test for a basic label syntax error
    #[test]
    fn test_syntax_err_1() {
        let code = r"
        label1:
        not_label
        label2:
          end
        ";
        assert_eq!(
            compile_error_display(code),
            "Error in test\n| syntax error at 3:1 near 'n' => expected label, instruction, or directive\n|   at 3:1 near 'n' => OneOf\n"
        );
    }

    #[test]
    fn test_local_labels() {
        let code = r#"
        .macro to_max(max)
          .option label_mode "local"
          label1:
            op add r1 r1 1
            jump $label:label1 lessThan r1 $max
        .endmacro

        label1:
          set r1 0
          to_max!(5)
          to_max!(10)
        "#;
        assert_eq!(
            compile_code(code),
            "set r1 0\nop add r1 r1 1\njump 1 lessThan r1 5\nop add r1 r1 1\njump 3 lessThan r1 10\n"
        );
    }

    #[test]
    fn test_duplicate_labels() {
        let code = r#"
        .macro m1()
          .option label_mode "local"
          ab:
            end
        .endmacro
        ab:
          set r1 1
        ab:
          set r2 2
        m1!()
        "#;
        assert_eq!(
            compile_fmt_error(code),
            "Many([PassError(ArcSource { name: \"test\", line: 9, column: 1, parent: None }, DuplicateLabelName(\"ab\"))])"
        );
    }

    #[test]
    fn test_include() {
        let code1 = r#"
        .include "inc.mblur"
        inc_macro!()
        "#;
        const CODE_2: &str = r#"
        .macro inc_macro()
          set a 1
          end
        .endmacro
        "#;

        use std::io;
        #[derive(Debug)]
        struct Api {}
        impl SystemApi for Api {
            fn resolve_path(&self, _base: &str, relative: &str) -> Result<String, io::Error> {
                match relative {
                    "inc.mblur" => Ok("./inc.mblur".into()),
                    _ => Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        relative.to_string(),
                    )),
                }
            }

            fn fs_read_to_string(&mut self, path: &str) -> Result<String, io::Error> {
                match path {
                    "./inc.mblur" => Ok(trim_indent(CODE_2, 8)),
                    _ => Err(io::Error::new(io::ErrorKind::NotFound, path.to_string())),
                }
            }
        }

        let mut comp = CompilerEnv::new(Api {});
        comp.register_builtin_macros().unwrap();
        let code1 = trim_indent(code1, MOD_INDENT);
        assert_eq!(
            comp.compile_and_generate("test", &code1).unwrap(),
            "set a 1\nend\n"
        );
    }

    #[test]
    fn test_include2() {
        use std::fs;
        println!("{:?}", std::env::current_dir());
        let abs_path = fs::canonicalize("./examples/tests/test_include.mblur")
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap();
        let mut comp = new_compiler();
        let code = fs::read_to_string(&abs_path).unwrap();
        assert_eq!(
            comp.compile_and_generate(&abs_path, &code).unwrap(),
            "set a 5\nend\n"
        );
    }

    #[test]
    fn test_if_blocks() {
        let code = r#"
        .define ab 5
        .if ab < 3
        set a 1
        .else
        set a 2
        .endif
        "#;
        assert_eq!(compile_code(code), "set a 2\n");
    }

    #[test]
    fn test_too_many_endifs() {
        let code = r#"
        .if defined("a")
          set $a 1
        .endif
        .endif
        "#;
        assert_eq!(
            compile_fmt_error(code),
            "PassError(ArcSource { name: \"test\", line: 5, column: 1, parent: None }, BlockStackUnexpectedType(\"root\", \"if or elseif\"))"
        );
    }

    #[test]
    fn test_too_many_ifs() {
        let code = r#"
        .if a
        .if b
        set c 1
        .endif
        "#;
        assert_eq!(
            compile_fmt_error(code),
            "PassError(ArcSource { name: \"test\", line: 0, column: 0, parent: None }, BlockStackUnexpectedType(\"if\", \"root\"))"
        );
    }

    #[test]
    fn readme_math_example() {
        let code = r#"
        sensor bx lancer1 @x
        sensor by lancer1 @y
        # These are math one-liners
        start:
        m! {
        dx = bx - @x
        dist = len(dx, by - @y)
        }
        print len
        # Conditional jump
        m! jump(print_out, len > 20)
        print " - far away"
        print_out:
        printflush message1
        # Unconditional jump
        m! jump(start)
        "#;
        assert_eq!(
            compile_code(code),
            "sensor bx lancer1 @x\nsensor by lancer1 @y\nop sub dx bx @x\nop sub __t0 by @y\nop len dist dx __t0\nprint len\njump 8 greaterThan len 20\nprint \" - far away\"\nprintflush message1\njump 2 always 1 1\n"
        );
    }

    #[test]
    fn readme_macro_example() {
        let code = r#"
        .macro shoot_control(controller)
        start:
        sensor shoot $controller @shooting
        sensor aimX $controller @shootX
        sensor aimY $controller @shootY
        set i 0
        fire_loop:
        getlink bldg i
        m! i += 1
        # Don't try to control the controller
        jump $label:fire_end equal bldg $controller
        control shoot bldg bldgAimX bldgAimY shoot 0
        fire_end:
        m! jump(fire_loop, i < @links)
        m! jump(start)
        .endmacro
        # Here lancer1 is the controller
        shoot_control!(lancer1)
        # Here salvo2 is the controller
        shoot_control!(salvo2)
        "#;
        assert_eq!(
            compile_code(code),
            "sensor shoot lancer1 @shooting\nsensor aimX lancer1 @shootX\nsensor aimY lancer1 @shootY\nset i 0\ngetlink bldg i\nop add i i 1\njump 8 equal bldg lancer1\ncontrol shoot bldg bldgAimX bldgAimY shoot 0\njump 4 lessThan i @links\njump 0 always 1 1\nsensor shoot salvo2 @shooting\nsensor aimX salvo2 @shootX\nsensor aimY salvo2 @shootY\nset i 0\ngetlink bldg i\nop add i i 1\njump 18 equal bldg salvo2\ncontrol shoot bldg bldgAimX bldgAimY shoot 0\njump 14 lessThan i @links\njump 10 always 1 1\n"
        );
    }

    #[test]
    fn web_demo() {
        let code = r#"
        # Minblur live demo!
          set i 1
        loop:
          op add i i 1
          m! jump(loop, (i % 1000) > 0)
          print i
          printflush message1
          m! jump(loop)"#;
        assert_eq!(
            compile_code(code),
            "set i 1\nop add i i 1\nop mod __t0 i 1000\njump 1 greaterThan __t0 0\nprint i\nprintflush message1\njump 1 always 1 1\n"
        );
    }

    #[test]
    fn test_example_memory() {
        let mut comp = new_compiler();
        let code = include_str!("../../examples/memory.mblur");
        insta::assert_display_snapshot!(comp.compile_and_generate("memory.mblur", code).unwrap());
    }

    #[test]
    fn example_units_1() {
        let mut comp = new_compiler();
        let code = include_str!("../../examples/units_1.mblur");
        let result = comp.generate_labels("units_1.mblur", code);
        // let result = comp.compile("units_1.mblur", code);
        let out = result
            .map(|statements| format!("Ok:\n{}", GenerateCode::new(&statements)))
            .unwrap_or_else(|e| format!("Error:\n{}", e));
        println!("{}", out);
        // Change to false to show output when running `cargo test`
        // panic!()
    }

    // #[test]
    fn _test_show_sizes() {
        use bn_expression::AValue;

        use crate::compiler::instruction::*;
        use crate::parser::statement;

        fn print_size<T: Sized>() {
            println!(
                "Size of {} = {}",
                std::any::type_name::<T>(),
                std::mem::size_of::<T>(),
            );
        }

        print_size::<statement::Directive>();
        print_size::<statement::Statement>();
        print_size::<statement::StatementData>();
        print_size::<AValue>();
        print_size::<InstValue>();
        print_size::<Instruction>();
        print_size::<InstructionRead>();
        print_size::<InstructionWrite>();
        print_size::<InstructionDraw>();
        print_size::<InstructionPrint>();
        print_size::<InstructionDrawFlush>();
        print_size::<InstructionPrintFlush>();
        print_size::<InstructionGetLink>();
        print_size::<InstructionControl>();
        print_size::<InstructionRadar>();
        print_size::<InstructionSensor>();
        print_size::<InstructionSet>();
        print_size::<InstructionOp>();
        print_size::<InstructionEnd>();
        print_size::<InstructionJump>();
        print_size::<InstructionUnitBind>();
        print_size::<InstructionUnitControl>();
        print_size::<InstructionUnitRadar>();
        print_size::<InstructionUnitLocate>();
        // assert!(false);
    }
}
