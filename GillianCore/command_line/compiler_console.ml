open Cmdliner
open Command_line_utils
open Utils.Syntaxes.Result

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t) : Console.S = struct
  open Common_args.Make (PC)

  let mode =
    let open Exec_mode in
    let doc = "Compile for Verification mode" in
    let verif =
      (Verification, Arg.info [ "ver"; "verif"; "verification" ] ~doc)
    in
    let doc = "Compile for Automatic Compositional Testing mode" in
    let act = (BiAbduction, Arg.info [ "bi"; "bi-abduction"; "act" ] ~doc) in
    let doc = "Compile for Concrete Execution mode" in
    let concrete = (Concrete, Arg.info [ "concrete"; "conc" ] ~doc) in
    let doc = "Compile for Whole Program Symbolic Testing mode" in
    let wpst =
      (Symbolic, Arg.info [ "symbolic"; "symb"; "wpst"; "stest" ] ~doc)
    in
    Arg.(last & vflag_all [ Verification ] [ concrete; wpst; verif; act ])

  let process_files files =
    let+ progs = PC.parse_and_compile_files files in
    let pp_annot fmt annot =
      Fmt.pf fmt "%a"
        (Yojson.Safe.pretty_print ?std:None)
        (PC.Annot.to_yojson annot)
    in
    List.iteri
      (fun i (path, prog) ->
        let init_data = if i = 0 then ID.to_yojson progs.init_data else `Null in
        burn_gil ~init_data
          ~pp_prog:(Prog.pp_labeled ~pp_annot)
          prog (Some path))
      progs.gil_progs

  (* SERVPIPS: [--servpips] / [--servpips-runtime DIR] compile the program
     exactly as [wpst --servpips [--servpips-runtime DIR]] runs it (the
     runtime preamble and the SERVPIPS compilation), so that a compile check
     sees the same program as the symbolic execution. *)
  let servpips_compile =
    let docs = "SERVPIPS OPTIONS" in
    let enabled =
      let doc =
        "Compile as $(b,wpst --servpips) does (SERVPIPS compilation; no event \
         log)."
      in
      Arg.(value & flag & info [ "servpips" ] ~docs ~doc)
    in
    let runtime =
      let doc =
        "SERVPIPS runtime directory containing the preamble.js used for \
         CommonJS programs, as for $(b,wpst --servpips-runtime). Implies \
         $(b,--servpips)."
      in
      Arg.(
        value
        & opt (some string) None
        & info [ "servpips-runtime" ] ~docs ~doc ~docv:"DIR")
    in
    let f enabled runtime = (enabled, runtime) in
    Term.(const f $ enabled $ runtime)

  let apply_servpips_compile (enabled, runtime) =
    match (enabled, runtime) with
    | false, None -> ()
    | _, Some d when not (Sys.file_exists d && Sys.is_directory d) ->
        Fmt.epr "gillian: --servpips-runtime: not a directory: %s@." d;
        exit 124
    | _, runtime_dir -> Servpips.enable_compile ~runtime_dir

  let compile files mode runtime_path ci tl_opts sp =
    let () = apply_servpips_compile sp in
    let () = Config.ci := ci in
    let () = PC.TargetLangOptions.apply tl_opts in
    let () = PC.initialize mode in
    let () = Config.current_exec_mode := mode in
    let () =
      Config.set_runtime_paths ?default_folders:PC.default_import_paths
        runtime_path
    in
    let r = process_files files in
    let () = exit_on_error r in
    exit 0

  let compile_t =
    Term.(
      const compile $ files $ mode $ runtime_path $ ci
      $ PC.TargetLangOptions.term $ servpips_compile)

  let compile_info =
    let doc = "Compiles a file from the target language to GIL" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Compiles a file to GIL. Target execution mode can be specified, \
           defaults to Verification.";
      ]
    in
    Cmd.info ~exits:Common_args.exit_code_info "compile" ~doc ~man

  let compile_cmd = Console.Normal (Cmd.v compile_info compile_t)
  let cmds = [ compile_cmd ]
end
