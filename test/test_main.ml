let () =
  Alcotest.run "glyph"
    [
      ("lexer", Test_lexer.tests);
      ("parser", Test_parser.tests);
      ("typecheck", Test_typecheck.tests);
      ("hir", Test_hir.tests);
      ("pattern", Test_pattern.tests);
      ("mir", Test_mir.tests);
      ("opt", Test_opt.tests);
      ("bytecode", Test_bytecode.tests);
      ("vm", Test_vm.tests);
      ("examples", Test_examples.tests);
      ("cli", Test_cli.tests);
      ("property", Test_property.tests);
    ]
