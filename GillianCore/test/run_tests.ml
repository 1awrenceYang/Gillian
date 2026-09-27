let test_suites : unit Alcotest.test list =
  [
    ("Gil_syntax.Reducers", Gil_syntax_tests.Visitors.tests);
    ("Servpips.Core", Servpips_core.tests);
  ]

let () = Alcotest.run "Gillian" test_suites
