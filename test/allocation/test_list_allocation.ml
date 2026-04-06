open! Base
open Expect_test_helpers_core

let%expect_test "is_prefix does not allocate" =
  let list = Sys.opaque_identity [ 1; 2; 3 ] in
  let prefix = Sys.opaque_identity [ 1; 2 ] in
  let equal = Int.equal in
  let (_ : bool) = require_no_allocation (fun () -> List.is_prefix list ~equal ~prefix) in
  [%expect {| |}]
;;

let%expect_test "is_suffix does not allocate" =
  let list = Sys.opaque_identity [ 1; 2; 3 ] in
  let suffix = Sys.opaque_identity [ 2; 3 ] in
  let equal = Int.equal in
  let (_ : bool) = require_no_allocation (fun () -> List.is_suffix list ~equal ~suffix) in
  [%expect {| |}]
;;

module%test From_local_iterators_to_be_replaced = struct
  module Test_result : sig
    type 'a t [@@deriving equal, sexp_of]

    val with_output : (local_ (Sexp.t -> unit) -> 'a) -> 'a t

    (** We test list-consuming functions by running them on every list in [examples] and
        logging both the output and the arguments passed to every invocation of the
        callback. *)
    val examples : int list list

    (** Examples for concat tests *)
    val nested_examples : int list list list

    (** Examples for unzip tests *)
    val zipped_examples : (int * int) list list

    val raised : _ t -> bool
  end = struct
    let examples = [ []; [ 1; 123; 12 ]; [ 0; 0; 0 ] ]

    let nested_examples =
      [ [ [] ]
      ; [ []; [] ]
      ; [ []; [ 0; 1 ] ]
      ; [ [ 1; 2 ]; [] ]
      ; [ [ 1 ]; [ 2; 3 ] ]
      ; [ [ 3; 2 ]; [ 1 ] ]
      ; [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
      ]
    ;;

    let zipped_examples = [ []; [ 1, 2 ]; [ 1, 2; 3, 4; 5, 6 ] ]
    let equal_exn _ _ = true

    type 'a t =
      { result : ('a, exn) Result.t
      ; outputs : Sexp.t list
      }
    [@@deriving equal, sexp_of]

    let with_output f =
      let outputs = ref [] in
      let output sexp = outputs := sexp :: !outputs in
      let result =
        match f output with
        | result -> Ok result
        | exception exn -> Error exn
      in
      { result; outputs = List.rev !outputs }
    ;;

    let raised t = Result.is_error t.result
  end

  let examples = Test_result.examples
  let nested_examples = Test_result.nested_examples
  let zipped_examples = Test_result.zipped_examples
  let append_local = (List.append [@alloc stack])

  module%test [@name "append_local"] _ = struct
    let append_local x y = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        append_local x y)
    ;;

    let%expect_test "append_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.cartesian_product examples examples
      |> List.iter ~f:(fun (example1, example2) ->
        let result = [%globalize: int list] (append_local example1 example2) in
        let expected = List.append example1 example2 in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        (1 123 12)
        (0 0 0)
        (1 123 12)
        (1 123 12 1 123 12)
        (1 123 12 0 0 0)
        (0 0 0)
        (0 0 0 1 123 12)
        (0 0 0 0 0 0)
        |}]
    ;;

    let%expect_test "append_local large list" =
      let input = List.range 0 2_000 in
      Expect_test_helpers_core.require_equal
        ~here:[%here]
        (module struct
          type t = int list [@@deriving equal, sexp_of]
        end)
        (append_local input input |> [%globalize: int list])
        (List.append input input)
    ;;
  end

  let concat_local = (List.concat [@alloc stack])

  module%test [@name "concat_local"] _ = struct
    let concat_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        concat_local x)
    ;;

    let%expect_test "concat_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.iter nested_examples ~f:(fun example ->
        let result = [%globalize: int list] (concat_local example) in
        let expected = List.concat example in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        ()
        (0 1)
        (1 2)
        (1 2 3)
        (3 2 1)
        (1 2 3 4 5 6 7 8 9)
        |}]
    ;;
  end

  let filteri_local = (List.filteri [@alloc stack])

  let%expect_test "filteri_local" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (filteri_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filteri_local example ~f:(fun _ i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.filteri example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123)))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let%expect_test "filteri_local large list" =
    let input = List.range 0 5_000 in
    let f _ i = i land 1 = 0 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (filteri_local input ~f |> [%globalize: int list])
      (List.filteri input ~f)
  ;;

  let filter_local = (List.filter [@alloc stack])

  let%expect_test "filter_local" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (filter_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filter_local example ~f:(fun i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.filter example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let filter_mapi_local = (List.filter_mapi [@mode local] [@alloc stack])

  let%expect_test "filter_mapi_local" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      Option.some_if (i > 50) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (filter_mapi_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filter_mapi_local example ~f:(fun index i ->
             if i > 50 then exclave_ Some (index + 1) else None))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.filter_mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123)))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let%expect_test "filter_mapi_local large list" =
    let input = List.range 0 5_000 in
    let f _ i = if i land 1 = 0 then Some (i + 1) else None in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (filter_mapi_local input ~f |> [%globalize: int list])
      (List.filter_mapi input ~f)
  ;;

  (* This allocates, so there's no no-allocation test. *)
  let filter_mapi_local_input = (List.filter_mapi [@mode local] [@alloc heap])

  let%expect_test "filter_mapi_local_input" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      Option.some_if (i > 50) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          filter_mapi_local_input example ~f:(f output) [@nontail])
      in
      let expected =
        Test_result.with_output (fun output ->
          List.filter_mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123)))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let filter_mapi_local_output = (List.filter_mapi [@mode global] [@alloc stack])

  let%expect_test "filter_mapi_local_output" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      Option.some_if (i > 50) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (filter_mapi_local_output example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filter_mapi_local_output example ~f:(fun index i ->
             if i > 50 then exclave_ Some (index + 1) else None))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.filter_mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123)))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let filter_map_local = (List.filter_map [@mode local] [@alloc stack])

  let%expect_test "filter_map_local" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Option.some_if (i > 10) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (filter_map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filter_map_local example ~f:(fun i ->
             if i > 10 then exclave_ Some (i + 1) else None))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.filter_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123 12))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let rev_filter_map_local = (List.rev_filter_map [@mode local] [@alloc stack])

  let%expect_test "rev_filter_map" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Option.some_if (i > 10) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list]
            (rev_filter_map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           rev_filter_map_local example ~f:(fun i ->
             if i > 10 then exclave_ Some (i + 1) else None))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.rev_filter_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (12 123))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  (* This allocates, so there's no no-allocation test. *)
  let filter_map_local_input = (List.filter_map [@mode local] [@alloc heap])

  let%expect_test "filter_map_local_input" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Option.some_if (i > 50) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          filter_map_local_input example ~f:(f output) [@nontail])
      in
      let expected =
        Test_result.with_output (fun output ->
          List.filter_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let filter_map_local_output = (List.filter_map [@mode global] [@alloc stack])

  let%expect_test "filter_map_local_output" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Option.some_if (i > 50) (Int.to_string i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (filter_map_local_output example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           filter_map_local_output example ~f:(fun i ->
             if i > 50 then exclave_ Some (i + 1) else None))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.filter_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let filter_opt_local = (List.filter_opt [@alloc stack])

  module%test [@name "filter_opt_local"] _ = struct
    let filter_opt_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        filter_opt_local x)
    ;;

    let%expect_test "filter_opt_local" =
      let module T = struct
        type t = string list [@@deriving equal, globalize, sexp_of]
      end
      in
      let f output i =
        output [%message "" (i : int)];
        Option.some_if (i > 50) (Int.to_string i)
      in
      List.iter examples ~f:(fun example ->
        let result =
          Test_result.with_output (fun output ->
            let example = (List.map [@mode local] [@alloc stack]) example ~f:(f output) in
            [%globalize: T.t] (filter_opt_local example) [@nontail])
        in
        let expected =
          Test_result.with_output (fun output ->
            let example = List.map example ~f:(f output :> _ -> _) in
            List.filter_opt example [@nontail])
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result));
      [%expect
        {|
        ((result (Ok ())) (outputs ()))
        ((result (Ok (123))) (outputs ((i 1) (i 123) (i 12))))
        ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
        |}]
    ;;
  end

  let concat_map_local = (List.concat_map [@mode local] [@alloc stack])

  let%expect_test "concat_map_local" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      [ i; i * i ]
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: int list] (concat_map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           concat_map_local example ~f:(fun i -> exclave_ [ i; i * i ]))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.concat_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (1 1 123 15_129 12 144))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (0 0 0 0 0 0))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let%expect_test "concat_map_local large list" =
    let input = List.range 0 2_000 in
    let f i = List.range 0 i in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (concat_map_local input ~f |> [%globalize: int list])
      (List.concat_map input ~f)
  ;;

  let partition_tf_local = (List.partition_tf [@alloc stack])

  let%expect_test "partition_tf_local" =
    let module T = struct
      type t = int list * int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          let ts_and_fs = partition_tf_local example ~f:(f output) in
          [%globalize: int list * int list] ts_and_fs [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           partition_tf_local example ~f:(fun i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.partition_tf example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok (() ()))) (outputs ()))
      ((result (Ok ((123) (1 12)))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (() (0 0 0)))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let%expect_test "partition_tf_local large list" =
    let input = List.range 0 10_000 in
    let f i = i land 1 = 0 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list * int list [@@deriving equal, sexp_of]
      end)
      (partition_tf_local input ~f |> [%globalize: int list * int list])
      (List.partition_tf input ~f)
  ;;

  let partition_map_local = (List.partition_map [@mode local] [@alloc stack])

  let%expect_test "partition_map_local" =
    let module T = struct
      type t = int list * int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i : _ Either.t =
      output [%message "" (i : int)];
      if i > 50 then First (i * 100) else Second (-i)
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          let mapped = partition_map_local example ~f:(f output) in
          [%globalize: int list * int list] mapped [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           partition_map_local example ~f:(fun i -> exclave_
             if i > 50 then First (i * 100) else Second (-i)))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.partition_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok (() ()))) (outputs ()))
      ((result (Ok ((12_300) (-1 -12)))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (() (0 0 0)))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let%expect_test "partition_map_local large list" =
    let input = List.range 0 10_000 in
    let f i : _ Either.t = if i land 1 = 0 then First (i + 1) else Second (-i) in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list * int list [@@deriving equal, sexp_of]
      end)
      (partition_map_local input ~f |> [%globalize: int list * int list])
      (List.partition_map input ~f)
  ;;

  let cartesian_product_local = (List.cartesian_product [@alloc stack])

  module%test [@name "cartesian_product_local"] _ = struct
    let cartesian_product_local x y = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        cartesian_product_local x y)
    ;;

    let%expect_test "cartesian_product_local" =
      let module T = struct
        type t = (int * int) list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.iter examples ~f:(fun example1 ->
        List.iter examples ~f:(fun example2 ->
          let result =
            [%globalize: (int * int) list] (cartesian_product_local example1 example2)
          in
          let expected = List.cartesian_product example1 example2 in
          Expect_test_helpers_core.require_equal
            (module struct
              type t = T.t [@@deriving equal, sexp_of]
            end)
            result
            expected;
          Core.print_s ([%sexp_of: T.t] result)));
      [%expect
        {|
        ()
        ()
        ()
        ()
        ((1 1) (1 123) (1 12) (123 1) (123 123) (123 12) (12 1) (12 123) (12 12))
        ((1 0) (1 0) (1 0) (123 0) (123 0) (123 0) (12 0) (12 0) (12 0))
        ()
        ((0 1) (0 123) (0 12) (0 1) (0 123) (0 12) (0 1) (0 123) (0 12))
        ((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
        |}]
    ;;

    let%expect_test "cartesian_product_local large list" =
      let input = List.range 0 2_000 in
      Expect_test_helpers_core.require_equal
        ~here:[%here]
        (module struct
          type t = (int * int) list [@@deriving equal, sexp_of]
        end)
        (cartesian_product_local input input |> [%globalize: (int * int) list])
        (List.cartesian_product input input)
    ;;
  end

  let find_local = (List.find [@mode local])

  let%expect_test "find_local" =
    let module T = struct
      type t = int option [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: int option] (find_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           find_local example ~f:(fun i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.find example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (123))) (outputs ((i 1) (i 123))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let find_local_exn = (List.find_exn [@mode local])

  let%expect_test "find_local_exn" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          find_local_exn example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           match find_local_exn example ~f:(fun i -> i > 50) with
           | exception _ -> 0
           | x -> x)
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.find_exn example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Error (Not_found_s "List.find_exn: not found"))) (outputs ()))
      ((result (Ok 123)) (outputs ((i 1) (i 123))))
      ((result (Error (Not_found_s "List.find_exn: not found")))
       (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let find_map_local = (List.find_map [@mode local local])

  let%expect_test "find_map_local" =
    let module T = struct
      type t = string option [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      match i > 50 with
      | false -> None
      | true -> Int.to_string (i + 10) |> Some
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string option] (find_map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           find_map_local example ~f:(fun i -> exclave_
             if i > 50 then Some (i + 10) else None))
         : int option);
      let expected =
        Test_result.with_output (fun output ->
          List.find_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (133))) (outputs ((i 1) (i 123))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let find_map_local_output = (List.find_map [@mode global local])

  let%expect_test "find_map_local_output" =
    let module T = struct
      type t = string option [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      match i > 50 with
      | false -> None
      | true -> Int.to_string (i + 10) |> Some
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string option]
            (find_map_local_output example ~f:(f output)) [@nontail])
      in
      let expected =
        Test_result.with_output (fun output ->
          List.find_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (133))) (outputs ((i 1) (i 123))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let findi_local = (List.findi [@mode local])

  let%expect_test "findi_local" =
    let module T = struct
      type t = (int * int) option [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: (int * int) option] (findi_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           find_map_local example ~f:(fun i -> exclave_
             if i > 50 then Some (i + 10) else None))
         : int option);
      let expected =
        Test_result.with_output (fun output ->
          List.findi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ((1 123)))) (outputs (((index 0) (i 1)) ((index 1) (i 123)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let findi_local_exn = (List.findi_exn [@mode local])

  let%expect_test "findi_local_exn" =
    let module T = struct
      type t = int * int [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i =
      output [%message "" (index : int) (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: int * int] (findi_local_exn example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           match findi_local_exn example ~f:(fun _ i -> i > 50) with
           | exception _ -> 0, 0
           | x -> x)
         : int * int);
      let expected =
        Test_result.with_output (fun output ->
          List.findi_exn example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Error (Not_found_s "List.findi_exn: not found"))) (outputs ()))
      ((result (Ok (1 123))) (outputs (((index 0) (i 1)) ((index 1) (i 123)))))
      ((result (Error (Not_found_s "List.findi_exn: not found")))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let fold_local = (List.fold [@mode local local])

  let%expect_test "fold_local" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output acc i =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_local example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_local example ~init ~f:(fun acc i -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold example ~init ~f:(fun acc i -> f output acc i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 1)) ((acc -9) (i 123)) ((acc 132) (i 12)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_local_accum = (List.fold [@mode global local])

  let%expect_test "fold_local_accum" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output acc i =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_local_accum example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_local_accum example ~init ~f:(fun acc i -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold example ~init ~f:(fun acc i -> f output acc i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 1)) ((acc -9) (i 123)) ((acc 132) (i 12)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_map_local = (List.fold_map [@mode local] [@alloc stack])

  let%expect_test "fold_map_local" =
    let module T = struct
      type t = int * int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output acc i =
      output [%message "" (acc : int) (i : int)];
      i - acc, acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          T.globalize (fold_map_local example ~init ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_map_local example ~init ~f:(fun acc i -> exclave_ i - acc, acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold_map example ~init ~f:(f output) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok (10 ()))) (outputs ()))
      ((result (Ok (-120 (10 -9 132))))
       (outputs (((acc 10) (i 1)) ((acc -9) (i 123)) ((acc 132) (i 12)))))
      ((result (Ok (-10 (10 -10 10))))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_local_input = (List.fold [@mode local global])

  let%expect_test "fold_local_input" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output acc i =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_local_input example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_local_input example ~init ~f:(fun acc i -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold example ~init ~f:(fun acc i -> f output acc i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 1)) ((acc -9) (i 123)) ((acc 132) (i 12)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_local_input_bits64 =
    (List.fold [@mode local global] [@kind value_or_null bits64])
  ;;

  let%expect_test "fold_local_input_bits64" =
    let open Unboxed in
    let module T = struct
      type t = Int64.t [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = #10L in
    let f output acc i =
      output [%message "" (acc : I64.t) (i : I64.t)];
      I64.O.(i - acc)
    in
    List.iter examples ~f:(fun example ->
      let example = List.map ~f:Int64.of_int example in
      let result =
        Test_result.with_output (fun output ->
          fold_local_input_bits64 example ~init ~f:(fun acc i ->
            f output acc (I64.unbox i))
          |> I64.box)
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_local_input_bits64 example ~init ~f:(fun acc i ->
             let i = I64.unbox i in
             I64.O.(i - acc))
           |> I64.box)
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold example ~init:(I64.box init) ~f:(fun acc i ->
            I64.box (f output (I64.unbox acc) (I64.unbox i)))
          [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 1)) ((acc -9) (i 123)) ((acc 132) (i 12)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let foldi_local = (List.foldi [@mode local local])

  let%expect_test "foldi_local" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output index acc i =
      output [%message "" (index : int) (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          foldi_local example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           foldi_local example ~init ~f:(fun _ acc i -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.foldi example ~init ~f:(fun index acc i -> f output index acc i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs
        (((index 0) (acc 10) (i 1)) ((index 1) (acc -9) (i 123))
         ((index 2) (acc 132) (i 12)))))
      ((result (Ok -10))
       (outputs
        (((index 0) (acc 10) (i 0)) ((index 1) (acc -10) (i 0))
         ((index 2) (acc 10) (i 0)))))
      |}]
  ;;

  let fold_right_local = (List.fold_right [@mode local local])

  let%expect_test "fold_right_local" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output i acc =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_right_local example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_right_local example ~init ~f:(fun i acc -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold_right example ~init ~f:(fun i acc -> f output i acc) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 12)) ((acc 2) (i 123)) ((acc 121) (i 1)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let%expect_test "fold_right_local large list" =
    let input = List.range 0 5_000 in
    let%template[@mode m = (global, local)] f x xs = x :: xs [@exclave_if_local m] in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (fold_right_local input ~init:[] ~f:(f [@mode local]) |> [%globalize: int list])
      (List.fold_right input ~init:[] ~f)
  ;;

  let fold_right_local_accum = (List.fold_right [@mode global local])

  let%expect_test "fold_right_local_accum" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output i acc =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_right_local_accum example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_right_local_accum example ~init ~f:(fun i acc -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold_right example ~init ~f:(fun i acc -> f output i acc) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 12)) ((acc 2) (i 123)) ((acc 121) (i 1)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_right_local_input = (List.fold_right [@mode local global])

  let%expect_test "fold_right_local_input" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output i acc =
      output [%message "" (acc : int) (i : int)];
      i - acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_right_local_input example ~init ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_right_local_input example ~init ~f:(fun i acc -> i - acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold_right example ~init ~f:(fun i acc -> f output i acc) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ()))
      ((result (Ok -120))
       (outputs (((acc 10) (i 12)) ((acc 2) (i 123)) ((acc 121) (i 1)))))
      ((result (Ok -10))
       (outputs (((acc 10) (i 0)) ((acc -10) (i 0)) ((acc 10) (i 0)))))
      |}]
  ;;

  let fold_until_local = (List.fold_until [@mode local local])

  let%expect_test "fold_until_local" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let init = 10 in
    let f output acc i =
      output [%message "f" (acc : int) (i : int)];
      let acc = acc + i + 1 in
      exclave_ if acc >= 100 then Continue_or_stop.Stop acc else Continue acc
    in
    let finish output acc =
      output [%message "finish" (acc : int)];
      acc
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          fold_until_local example ~init ~f:(f output) ~finish:(finish output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           fold_until_local example ~init ~finish:Fn.id ~f:(fun acc i -> exclave_
             let acc = acc + i + 1 in
             if acc >= 100 then Continue_or_stop.Stop acc else Continue acc))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.fold_until
            example
            ~init
            ~f:(fun acc i ->
              match f output acc i with
              | Continue a -> Continue a
              | Stop a -> Stop a)
            ~finish:(fun acc -> finish output acc) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 10)) (outputs ((finish (acc 10)))))
      ((result (Ok 136)) (outputs ((f (acc 10) (i 1)) (f (acc 12) (i 123)))))
      ((result (Ok 13))
       (outputs
        ((f (acc 10) (i 0)) (f (acc 11) (i 0)) (f (acc 12) (i 0))
         (finish (acc 13)))))
      |}]
  ;;

  let reduce_local = (List.reduce [@mode local])

  let%expect_test "reduce_local" =
    let module T = struct
      type t = int option [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output a b =
      output [%message "" (a : int) (b : int)];
      a + b
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (reduce_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           reduce_local example ~f:(fun a b -> a + b))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.reduce example ~f:(fun a b -> f output a b) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (136))) (outputs (((a 1) (b 123)) ((a 124) (b 12)))))
      ((result (Ok (0))) (outputs (((a 0) (b 0)) ((a 0) (b 0)))))
      |}]
  ;;

  let reduce_local_exn = (List.reduce_exn [@mode local])

  let%expect_test "reduce_local_exn" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output a b =
      output [%message "" (a : int) (b : int)];
      a + b
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t] (reduce_local_exn example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           match reduce_local_exn example ~f:(fun a b -> a + b) with
           | exception _ -> 0
           | x -> x)
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.reduce_exn example ~f:(fun a b -> f output a b) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Error (Invalid_argument List.reduce_exn))) (outputs ()))
      ((result (Ok 136)) (outputs (((a 1) (b 123)) ((a 124) (b 12)))))
      ((result (Ok 0)) (outputs (((a 0) (b 0)) ((a 0) (b 0)))))
      |}]
  ;;

  module type Summable = Container.Summable [@mode local]

  let sum_local = (List.sum [@mode local local])

  let%expect_test "sum_local" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i * i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          sum_local (module Int) example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           sum_local (module Int) example ~f:(fun i -> i * i))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.sum (module Int) example ~f:(f output) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok 0)) (outputs ()))
      ((result (Ok 15_274)) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok 0)) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let init_local = (List.init [@alloc stack])

  let%expect_test "init_local" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i
    in
    List.iter [ 0; 1; 10 ] ~f:(fun example_len ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: int list] (init_local example_len ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           init_local example_len ~f:Fn.id)
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.init example_len ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (0))) (outputs ((i 0))))
      ((result (Ok (0 1 2 3 4 5 6 7 8 9)))
       (outputs ((i 9) (i 8) (i 7) (i 6) (i 5) (i 4) (i 3) (i 2) (i 1) (i 0))))
      |}]
  ;;

  let init_local_i64 = (List.init_i64 [@alloc stack])

  let%expect_test "init_local_i64" =
    let open Unboxed in
    let module T = struct
      type t = Int64.t list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : I64.t)];
      I64.box i
    in
    List.iter [ 0L; 1L; 10L ] ~f:(fun example_len ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: T.t]
            (init_local_i64 (I64.unbox example_len) ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           init_local_i64 (I64.unbox example_len) ~f:(fun x -> exclave_ I64.box x))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.init
            (Int64.to_int_trunc example_len)
            ~f:(fun i -> f output (I64.of_int i) :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (0))) (outputs ((i 0))))
      ((result (Ok (0 1 2 3 4 5 6 7 8 9)))
       (outputs ((i 9) (i 8) (i 7) (i 6) (i 5) (i 4) (i 3) (i 2) (i 1) (i 0))))
      |}]
  ;;

  let is_empty = List.is_empty

  module%test [@name "is_empty"] _ = struct
    let is_empty x =
      Expect_test_helpers_core.require_no_allocation (fun () -> is_empty x) [@nontail]
    ;;

    let%expect_test "is_empty" =
      List.iter examples ~f:(fun example ->
        assert (Bool.equal (is_empty example) (List.is_empty example)))
    ;;
  end

  let iter_local = (List.iter [@mode local])

  let%expect_test "iter_local" =
    let module T = struct
      type t = unit [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i = output [%message "" (i : int)] in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          iter_local example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           iter_local example ~f:(fun _ -> ()))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.iter example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ())) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok ())) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let iteri_local = (List.iteri [@mode local])

  let%expect_test "iteri_local" =
    let module T = struct
      type t = unit [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index i = output [%message "" (index : int) (i : int)] in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          iteri_local example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           iteri_local example ~f:(fun _ _ -> ()))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.iteri example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ()))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ()))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let length = List.length

  module%test [@name "length"] _ = struct
    let length x =
      Expect_test_helpers_core.require_no_allocation (fun () -> length x) [@nontail]
    ;;

    let%expect_test "length" =
      List.iter examples ~f:(fun example ->
        assert ([%equal: int] (length example) (List.length example)));
      [%expect {| |}]
    ;;
  end

  let map_local = (List.map [@mode local] [@alloc stack])

  let%expect_test "map_local" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Int.to_string i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           map_local example ~f:Fn.id)
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (1 123 12))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (0 0 0))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  (* This allocates, so there's no no-allocation test. *)
  let map_local_input = (List.map [@mode local] [@alloc heap])

  let%expect_test "map_local_input" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Int.to_string i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          map_local_input example ~f:(f output) [@nontail])
      in
      let expected =
        Test_result.with_output (fun output ->
          List.map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (1 123 12))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (0 0 0))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let map_local_output = (List.map [@mode global] [@alloc stack])

  let%expect_test "map_local_output" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Int.to_string i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (map_local_output example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           map_local_output example ~f:Fn.id)
         : int list);
      let expected =
        Test_result.with_output (fun output -> List.map example ~f:(f output) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (1 123 12))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (0 0 0))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let mapi_local = (List.mapi [@mode local] [@alloc stack])

  let%expect_test "mapi_local" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index (i : int) =
      output [%message "" (index : int) (i : int)];
      Core.sprintf "%d %d" index i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (mapi_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           mapi_local example ~f:(fun _ x -> x))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ("0 1" "1 123" "2 12")))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ("0 0" "1 0" "2 0")))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let%expect_test "mapi_local large list" =
    let input = List.range 0 5_000 in
    let f _ i = i + 1 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (mapi_local input ~f |> [%globalize: int list])
      (List.mapi input ~f)
  ;;

  (* This allocates, so there's no no-allocation test. *)
  let mapi_local_input = (List.mapi [@mode local] [@alloc heap])

  let%expect_test "mapi_local_input" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index (i : int) =
      output [%message "" (index : int) (i : int)];
      Core.sprintf "%d %d" index i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          mapi_local_input example ~f:(f output) [@nontail])
      in
      let expected =
        Test_result.with_output (fun output ->
          List.mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ("0 1" "1 123" "2 12")))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ("0 0" "1 0" "2 0")))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let mapi_local_output = (List.mapi [@mode global] [@alloc stack])

  let%expect_test "mapi_local_output" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output index (i : int) =
      output [%message "" (index : int) (i : int)];
      Core.sprintf "%d %d" index i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (mapi_local_output example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           mapi_local_output example ~f:(fun _ x -> x))
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.mapi example ~f:(fun index i -> f output index i) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ("0 1" "1 123" "2 12")))
       (outputs (((index 0) (i 1)) ((index 1) (i 123)) ((index 2) (i 12)))))
      ((result (Ok ("0 0" "1 0" "2 0")))
       (outputs (((index 0) (i 0)) ((index 1) (i 0)) ((index 2) (i 0)))))
      |}]
  ;;

  let map2_exn_local = (List.map2_exn [@mode local] [@alloc stack])

  let%expect_test "map2_exn_local " =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output a b =
      output [%message "" (a : int) (b : int)];
      [%string "%{a#Int},%{b#Int}"]
    in
    List.iter examples ~f:(fun example1 ->
      List.iter examples ~f:(fun example2 ->
        let result =
          Test_result.with_output (fun output ->
            [%globalize: string list]
              (map2_exn_local example1 example2 ~f:(f output)) [@nontail])
        in
        (* [map2_exn_local] allocates when it raises. *)
        if not (Test_result.raised result)
        then
          (* printing allocates, so try that again without actually printing anything just
             to check for allocation *)
          ignore
            (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
               map2_exn_local example1 example2 ~f:(fun a b -> a + b))
             : int list);
        let expected =
          Test_result.with_output (fun output ->
            List.map2_exn example1 example2 ~f:(fun a b -> f output a b) [@nontail])
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result)));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in map2_exn: 0 <> 3")))
       (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in map2_exn: 0 <> 3")))
       (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in map2_exn: 3 <> 0")))
       (outputs ()))
      ((result (Ok (1,1 123,123 12,12)))
       (outputs (((a 1) (b 1)) ((a 123) (b 123)) ((a 12) (b 12)))))
      ((result (Ok (1,0 123,0 12,0)))
       (outputs (((a 1) (b 0)) ((a 123) (b 0)) ((a 12) (b 0)))))
      ((result (Error (Invalid_argument "length mismatch in map2_exn: 3 <> 0")))
       (outputs ()))
      ((result (Ok (0,1 0,123 0,12)))
       (outputs (((a 0) (b 1)) ((a 0) (b 123)) ((a 0) (b 12)))))
      ((result (Ok (0,0 0,0 0,0)))
       (outputs (((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)))))
      |}]
  ;;

  let%expect_test "map2_exn_local large list" =
    let input = List.range 0 5_000 in
    let f i j = i + j in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (map2_exn_local input input ~f |> [%globalize: int list])
      (List.map2_exn input input ~f)
  ;;

  let zip_exn_local = (List.zip_exn [@alloc stack])

  let%expect_test "zip_exn_local " =
    let module T = struct
      type t = (int * int) list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output l =
      output [%message "" (l : int list)];
      l
    in
    List.iter examples ~f:(fun example1 ->
      List.iter examples ~f:(fun example2 ->
        let result =
          Test_result.with_output (fun output ->
            [%globalize: (int * int) list]
              (zip_exn_local (f output example1) (f output example2)) [@nontail])
        in
        (* [zip_exn_local] allocates when it raises. *)
        if not (Test_result.raised result)
        then
          (* printing allocates, so try that again without actually printing anything just
             to check for allocation *)
          ignore
            (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
               zip_exn_local example1 example2)
             : T.t);
        let expected =
          Test_result.with_output (fun output ->
            List.zip_exn (f output example1) (f output example2))
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result)));
    [%expect
      {|
      ((result (Ok ())) (outputs ((l ()) (l ()))))
      ((result (Error (Invalid_argument "length mismatch in zip_exn: 0 <> 3")))
       (outputs ((l (1 123 12)) (l ()))))
      ((result (Error (Invalid_argument "length mismatch in zip_exn: 0 <> 3")))
       (outputs ((l (0 0 0)) (l ()))))
      ((result (Error (Invalid_argument "length mismatch in zip_exn: 3 <> 0")))
       (outputs ((l ()) (l (1 123 12)))))
      ((result (Ok ((1 1) (123 123) (12 12))))
       (outputs ((l (1 123 12)) (l (1 123 12)))))
      ((result (Ok ((1 0) (123 0) (12 0)))) (outputs ((l (0 0 0)) (l (1 123 12)))))
      ((result (Error (Invalid_argument "length mismatch in zip_exn: 3 <> 0")))
       (outputs ((l ()) (l (0 0 0)))))
      ((result (Ok ((0 1) (0 123) (0 12)))) (outputs ((l (1 123 12)) (l (0 0 0)))))
      ((result (Ok ((0 0) (0 0) (0 0)))) (outputs ((l (0 0 0)) (l (0 0 0)))))
      |}]
  ;;

  let nth_local = (List.nth [@mode local])

  module%test [@name "nth_local"] _ = struct
    let nth_local x y = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        nth_local x y)
    ;;

    let%expect_test "nth_local" =
      let module T = struct
        type t = int option list [@@deriving equal, globalize, sexp_of]
      end
      in
      let f output n =
        output [%message "" (n : int)];
        n
      in
      List.iter examples ~f:(fun example ->
        let result =
          Test_result.with_output (fun output ->
            List.map [ 0; 1; 2 ] ~f:(fun n ->
              [%globalize: int option] (nth_local example (f output n)) [@nontail])
            [@nontail])
        in
        let expected =
          Test_result.with_output (fun output ->
            List.map [ 0; 1; 2 ] ~f:(fun n -> List.nth example (f output n)) [@nontail])
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result));
      [%expect
        {|
        ((result (Ok (() () ()))) (outputs ((n 0) (n 1) (n 2))))
        ((result (Ok ((1) (123) (12)))) (outputs ((n 0) (n 1) (n 2))))
        ((result (Ok ((0) (0) (0)))) (outputs ((n 0) (n 1) (n 2))))
        |}]
    ;;
  end

  let nth_local_exn = (List.nth_exn [@mode local])

  let%expect_test "nth_local_exn" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output n =
      output [%message "" (n : int)];
      n
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          List.map [ 0; 1; 2 ] ~f:(fun n ->
            [%globalize: int] (nth_local_exn example (f output n)))
          [@nontail])
      in
      (* [nth_local_exn] allocates when it raises. *)
      if not (Test_result.raised result)
      then
        (* printing allocates, so try that again without actually printing anything just
           to check for allocation *)
        ignore
          (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
             map_local [ 0; 1; 2 ] ~f:(fun n -> exclave_ nth_local_exn example n))
           : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.map [ 0; 1; 2 ] ~f:(fun n -> List.nth_exn example (f output n)) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result
        (Error (Invalid_argument "List.nth_exn 0 called on list of length 0")))
       (outputs ((n 0))))
      ((result (Ok (1 123 12))) (outputs ((n 0) (n 1) (n 2))))
      ((result (Ok (0 0 0))) (outputs ((n 0) (n 1) (n 2))))
      |}]
  ;;

  let hd_exn = (List.hd_exn [@mode local])

  let%expect_test "hd_exn" =
    let module T = struct
      type t = int [@@deriving equal, globalize, sexp_of]
    end
    in
    List.iter examples ~f:(fun example ->
      let printed =
        Test_result.with_output (fun _ -> [%globalize: T.t] (hd_exn example) [@nontail])
      in
      Core.print_s ([%sexp_of: T.t Test_result.t] printed);
      let local =
        Or_error.try_with (fun () -> [%globalize: T.t] (hd_exn example) [@nontail])
      in
      let base = Or_error.try_with (fun () -> List.hd_exn example) in
      Expect_test_helpers_core.require_equal
        (module Bool)
        (Result.is_ok local)
        (Result.is_ok base);
      match local, base with
      | Ok x, Ok y -> Expect_test_helpers_core.require_equal (module Int) x y
      | Error _, Error _ -> ()
      | _ -> assert false);
    [%expect
      {|
      ((result (Error (Failure hd))) (outputs ()))
      ((result (Ok 1)) (outputs ()))
      ((result (Ok 0)) (outputs ()))
      |}]
  ;;

  let tl_exn = (List.tl_exn [@mode local])

  let%expect_test "tl_exn" =
    let module T = struct
      type t = int list [@@deriving equal, globalize, sexp_of]
    end
    in
    List.iter examples ~f:(fun example ->
      let printed =
        Test_result.with_output (fun _ -> [%globalize: T.t] (tl_exn example) [@nontail])
      in
      Core.print_s ([%sexp_of: T.t Test_result.t] printed);
      let local =
        Or_error.try_with (fun () -> [%globalize: T.t] (tl_exn example) [@nontail])
      in
      let base = Or_error.try_with (fun () -> List.tl_exn example) in
      Expect_test_helpers_core.require_equal
        (module Bool)
        (Result.is_ok local)
        (Result.is_ok base);
      match local, base with
      | Ok x, Ok y ->
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          x
          y
      | Error _, Error _ -> ()
      | _ -> assert false);
    [%expect
      {|
      ((result (Error (Failure tl))) (outputs ()))
      ((result (Ok (123 12))) (outputs ()))
      ((result (Ok (0 0))) (outputs ()))
      |}]
  ;;

  let chunks_of t ~length = exclave_ (List.chunks_of [@alloc stack]) t ~length

  let%expect_test "chunks_of" =
    let module T = struct
      type t = int list list [@@deriving equal, globalize, sexp_of]
    end
    in
    List.iter examples ~f:(fun example ->
      List.iter [ 1; 2; 3 ] ~f:(fun length ->
        let result = [%globalize: T.t] (chunks_of example ~length) in
        let expected = List.chunks_of example ~length in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        ignore
          (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
             chunks_of example ~length)
           : T.t)))
  ;;

  let%expect_test "chunks_of invalid length" =
    let module T = struct
      type t = int list list [@@deriving equal, globalize, sexp_of]
    end
    in
    List.iter examples ~f:(fun example ->
      List.iter [ 0; -1 ] ~f:(fun length ->
        let result =
          Test_result.with_output (fun _output ->
            [%globalize: T.t] (chunks_of example ~length) [@nontail])
        in
        let expected =
          Test_result.with_output (fun _output ->
            List.chunks_of example ~length [@nontail])
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected))
  ;;

  let%expect_test "chunks_of large list, big split" =
    let input = List.range 0 10_000 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list list [@@deriving equal, sexp_of]
      end)
      (chunks_of input ~length:5_000 |> [%globalize: int list list])
      (List.chunks_of input ~length:5_000)
  ;;

  let%expect_test "chunks_of large list, little split" =
    let input = List.range 0 10_000 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list list [@@deriving equal, sexp_of]
      end)
      (chunks_of input ~length:2 |> [%globalize: int list list])
      (List.chunks_of input ~length:2)
  ;;

  let%expect_test "chunks_of large list, large split" =
    let input = List.range 0 (1024 * 1024) in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list list [@@deriving equal, sexp_of]
      end)
      (chunks_of input ~length:1024 |> [%globalize: int list list])
      (List.chunks_of input ~length:1024)
  ;;

  let rev_local = (List.rev [@alloc stack])

  module%test [@name "rev_local"] _ = struct
    let rev_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        rev_local x)
    ;;

    let%expect_test "rev_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.iter examples ~f:(fun example ->
        let result = [%globalize: int list] (rev_local example) in
        let expected = List.rev example in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        (12 123 1)
        (0 0 0)
        |}]
    ;;
  end

  let take_local = (List.take [@alloc stack])

  module%test [@name "take_local"] _ = struct
    let take_local x y = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        take_local x y)
    ;;

    let%expect_test "take_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.iter examples ~f:(fun example ->
        let result = [%globalize: int list] (take_local example 2) in
        let expected = List.take example 2 in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        (1 123)
        (0 0)
        |}]
    ;;

    let%expect_test "take_local large list" =
      let input = List.range 0 10_000 in
      Expect_test_helpers_core.require_equal
        ~here:[%here]
        (module struct
          type t = int list [@@deriving equal, sexp_of]
        end)
        (take_local input 5_000 |> [%globalize: int list])
        (List.take input 5_000)
    ;;
  end

  let exists_local = (List.exists [@mode local])

  let%expect_test "exists_local" =
    let module T = struct
      type t = bool [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          exists_local example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           exists_local example ~f:(fun i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.exists example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok false)) (outputs ()))
      ((result (Ok true)) (outputs ((i 1) (i 123))))
      ((result (Ok false)) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let for_all_local = (List.for_all [@mode local])

  let%expect_test "for_all_local" =
    let module T = struct
      type t = bool [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      i > 50
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          for_all_local example ~f:(f output) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           for_all_local example ~f:(fun i -> i > 50))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.for_all example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok true)) (outputs ()))
      ((result (Ok false)) (outputs ((i 1))))
      ((result (Ok false)) (outputs ((i 0))))
      |}]
  ;;

  let iter2_exn_local = (List.iter2_exn [@mode local])

  let%expect_test "iter2_exn_local" =
    let module T = struct
      type t = unit [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output a b = output [%message "" (a : int) (b : int)] in
    List.iter examples ~f:(fun example1 ->
      List.iter examples ~f:(fun example2 ->
        let result =
          Test_result.with_output (fun output ->
            iter2_exn_local example1 example2 ~f:(f output) [@nontail])
        in
        (* [iter2_exn_local] allocates when it raises. *)
        if not (Test_result.raised result)
        then
          (* printing allocates, so try that again without actually printing anything just
             to check for allocation *)
          ignore
            (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
               iter2_exn_local example1 example2 ~f:(fun _ _ -> ()))
             : T.t);
        let expected =
          Test_result.with_output (fun output ->
            List.iter2_exn example1 example2 ~f:(fun index i -> f output index i)
            [@nontail])
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result)));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in iter2_exn: 0 <> 3")))
       (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in iter2_exn: 0 <> 3")))
       (outputs ()))
      ((result (Error (Invalid_argument "length mismatch in iter2_exn: 3 <> 0")))
       (outputs ()))
      ((result (Ok ()))
       (outputs (((a 1) (b 1)) ((a 123) (b 123)) ((a 12) (b 12)))))
      ((result (Ok ())) (outputs (((a 1) (b 0)) ((a 123) (b 0)) ((a 12) (b 0)))))
      ((result (Error (Invalid_argument "length mismatch in iter2_exn: 3 <> 0")))
       (outputs ()))
      ((result (Ok ())) (outputs (((a 0) (b 1)) ((a 0) (b 123)) ((a 0) (b 12)))))
      ((result (Ok ())) (outputs (((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)))))
      |}]
  ;;

  let split_n_local xs n = exclave_
    let a, b = (List.split_n [@alloc stack]) xs n in
    #(a, b)
  ;;

  let%expect_test "split_n_local" =
    let module T = struct
      type t = (int list * int list) list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output n =
      output [%message "" (n : int)];
      n
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          List.map [ 0; 1; 2 ] ~f:(fun n ->
            let #(a, b) = split_n_local example (f output n) in
            [%globalize: int list * int list] (a, b) [@nontail])
          [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           map_local [ 0; 1; 2 ] ~f:(fun n -> exclave_
             let #(a, b) = split_n_local example n in
             a, b))
         : T.t);
      let expected =
        Test_result.with_output (fun output ->
          List.map [ 0; 1; 2 ] ~f:(fun n -> List.split_n example (f output n)) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ((() ()) (() ()) (() ())))) (outputs ((n 0) (n 1) (n 2))))
      ((result (Ok ((() (1 123 12)) ((1) (123 12)) ((1 123) (12)))))
       (outputs ((n 0) (n 1) (n 2))))
      ((result (Ok ((() (0 0 0)) ((0) (0 0)) ((0 0) (0)))))
       (outputs ((n 0) (n 1) (n 2))))
      |}]
  ;;

  let%expect_test "split_n_local large list" =
    let input = List.range 0 10_000 in
    let #(a, b) = split_n_local input 5_000 in
    Expect_test_helpers_core.require_equal
      ~here:[%here]
      (module struct
        type t = int list * int list [@@deriving equal, sexp_of]
      end)
      ([%globalize: int list * int list] (a, b))
      (List.split_n input 5_000)
  ;;

  let rev_append_local = (List.rev_append [@alloc stack])

  module%test [@name "rev_append_local"] _ = struct
    let rev_append_local x y = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        rev_append_local x y)
    ;;

    let%expect_test "rev_append_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.cartesian_product examples examples
      |> List.iter ~f:(fun (example1, example2) ->
        let result = [%globalize: int list] (rev_append_local example1 example2) in
        let expected = List.rev_append example1 example2 in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        (1 123 12)
        (0 0 0)
        (12 123 1)
        (12 123 1 1 123 12)
        (12 123 1 0 0 0)
        (0 0 0)
        (0 0 0 1 123 12)
        (0 0 0 0 0 0)
        |}]
    ;;
  end

  let rev_map_local = (List.rev_map [@mode local] [@alloc stack])

  let%expect_test "rev_map_local" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f output i =
      output [%message "" (i : int)];
      Int.to_string i
    in
    List.iter examples ~f:(fun example ->
      let result =
        Test_result.with_output (fun output ->
          [%globalize: string list] (rev_map_local example ~f:(f output)) [@nontail])
      in
      (* printing allocates, so try that again without actually printing anything just to
         check for allocation *)
      ignore
        (Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
           rev_map_local example ~f:Fn.id)
         : int list);
      let expected =
        Test_result.with_output (fun output ->
          List.rev_map example ~f:(f output :> _ -> _) [@nontail])
      in
      Expect_test_helpers_core.require_equal
        (module struct
          type t = T.t Test_result.t [@@deriving equal, sexp_of]
        end)
        result
        expected;
      Core.print_s ([%sexp_of: T.t Test_result.t] result));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok (12 123 1))) (outputs ((i 1) (i 123) (i 12))))
      ((result (Ok (0 0 0))) (outputs ((i 0) (i 0) (i 0))))
      |}]
  ;;

  let dedup_and_sort_local = (List.dedup_and_sort [@alloc stack])

  module%test [@name "dedup_and_sort_local"] _ = struct
    let dedup_and_sort_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        dedup_and_sort_local x)
    ;;

    let%expect_test "dedup_and_sort_local" =
      let module T = struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end
      in
      List.iter examples ~f:(fun example ->
        let result = T.globalize (dedup_and_sort_local example ~compare) in
        let expected = List.dedup_and_sort example ~compare in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t] result));
      [%expect
        {|
        ()
        (1 12 123)
        (0)
        |}]
    ;;
  end

  let group_local = (List.group [@alloc stack])

  module%test [@name "group_local"] _ = struct
    let group_local x ~break = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        group_local x ~break)
    ;;

    let%expect_test "group_local" =
      let breaks =
        [ (fun a b -> b - a > 100)
        ; (fun _ b -> b < 100)
        ; (fun _ _ -> true)
        ; (fun _ _ -> false)
        ]
      in
      List.iter breaks ~f:(fun break ->
        let module T = struct
          type t = int list list [@@deriving equal, globalize, sexp_of]
        end
        in
        List.iter examples ~f:(fun example ->
          let result =
            [%globalize: int list list]
              (group_local example ~break:(fun a b -> break a b))
          in
          let expected = List.group example ~break in
          Expect_test_helpers_core.require_equal
            (module struct
              type t = T.t [@@deriving equal, sexp_of]
            end)
            result
            expected;
          Core.print_s ([%sexp_of: T.t] result)));
      [%expect
        {|
        ()
        ((1) (123 12))
        ((0 0 0))
        ()
        ((1 123) (12))
        ((0) (0) (0))
        ()
        ((1) (123) (12))
        ((0) (0) (0))
        ()
        ((1 123 12))
        ((0 0 0))
        |}]
    ;;
  end

  let sort_local = (List.sort [@alloc stack])

  module%test [@name "sort_local"] _ = struct
    let sort_local x ~compare = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        sort_local x ~compare)
    ;;

    let%expect_test "sort_local" =
      let compares =
        [ Int.descending; Int.ascending; (fun _ _ -> 0); (fun _ _ -> -1); (fun _ _ -> 1) ]
      in
      (* Compare for correctness *)
      List.iter compares ~f:(fun compare ->
        let module T = struct
          type t = int list [@@deriving equal, globalize, sexp_of]
        end
        in
        List.iter examples ~f:(fun example ->
          let result =
            [%globalize: int list] (sort_local example ~compare:(fun a b -> compare a b))
          in
          let expected = List.sort example ~compare in
          Expect_test_helpers_core.require_equal
            (module struct
              type t = T.t [@@deriving equal, sexp_of]
            end)
            result
            expected;
          Core.print_s ([%sexp_of: T.t] result)));
      [%expect
        {|
        ()
        (123 12 1)
        (0 0 0)
        ()
        (1 12 123)
        (0 0 0)
        ()
        (1 123 12)
        (0 0 0)
        ()
        (1 123 12)
        (0 0 0)
        ()
        (12 123 1)
        (0 0 0)
        |}]
    ;;
  end

  let transpose_local = (List.transpose [@alloc stack])

  module%test [@name "transpose_local"] _ = struct
    let transpose_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        transpose_local x)
    ;;

    let%expect_test "transpose_local" =
      List.iter nested_examples ~f:(fun example ->
        let result =
          [%globalize: int list list option]
            (transpose_local example [@nontail]) [@nontail]
        in
        let expected = List.transpose example in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = int list list option [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: int list list option] result));
      [%expect
        {|
        (())
        (())
        ()
        ()
        ()
        ()
        (((1 4 7) (2 5 8) (3 6 9)))
        |}]
    ;;
  end

  let unzip_local = (List.unzip [@alloc stack])

  module%test [@name "unzip_local"] _ = struct
    let unzip_local x = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        unzip_local x)
    ;;

    let%expect_test "unzip_local" =
      List.iter zipped_examples ~f:(fun example ->
        let result =
          [%globalize: int list * int list] (unzip_local example [@nontail]) [@nontail]
        in
        let expected = List.unzip example in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = int list * int list [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: int list * int list] result));
      [%expect
        {|
        (() ())
        ((1) (2))
        ((1 3 5) (2 4 6))
        |}]
    ;;

    let%expect_test "unzip_local large list" =
      let input = List.range 0 5_000 in
      let input = List.zip_exn input (List.rev input) in
      Expect_test_helpers_core.require_equal
        ~here:[%here]
        (module struct
          type t = int list * int list [@@deriving equal, sexp_of]
        end)
        (unzip_local input |> [%globalize: int list * int list])
        (List.unzip input)
    ;;
  end

  let min_elt_local = (List.min_elt [@mode local])

  module%test [@name "min_elt_local"] _ = struct
    let min_elt_local x ~compare = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        min_elt_local x ~compare)
    ;;

    let%expect_test "min_elt_local" =
      let compares =
        [ Int.descending; Int.ascending; (fun _ _ -> 0); (fun _ _ -> -1); (fun _ _ -> 1) ]
      in
      List.iter compares ~f:(fun compare ->
        let module T = struct
          type t = int option [@@deriving equal, globalize, sexp_of]
        end
        in
        List.iter examples ~f:(fun example ->
          let result =
            [%globalize: T.t]
              (min_elt_local
                 example
                 ~compare:(compare :> local_ int -> local_ int -> int))
          in
          let expected = List.min_elt example ~compare in
          Expect_test_helpers_core.require_equal
            (module struct
              type t = T.t [@@deriving equal, sexp_of]
            end)
            result
            expected))
    ;;
  end

  let max_elt_local = (List.max_elt [@mode local])

  module%test [@name "max_elt_local"] _ = struct
    let max_elt_local x ~compare = exclave_
      Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
        max_elt_local x ~compare)
    ;;

    let%expect_test "max_elt_local" =
      let compares =
        [ Int.descending; Int.ascending; (fun _ _ -> 0); (fun _ _ -> -1); (fun _ _ -> 1) ]
      in
      List.iter compares ~f:(fun compare ->
        let module T = struct
          type t = int option [@@deriving equal, globalize, sexp_of]
        end
        in
        List.iter examples ~f:(fun example ->
          let result =
            [%globalize: T.t]
              (max_elt_local
                 example
                 ~compare:(compare :> local_ int -> local_ int -> int))
          in
          let expected = List.max_elt example ~compare in
          Expect_test_helpers_core.require_equal
            (module struct
              type t = T.t [@@deriving equal, sexp_of]
            end)
            result
            expected))
    ;;
  end

  module Let_syntax = List.Local.Let_syntax

  let%expect_test "Let_syntax" =
    let module T = struct
      type t = string list [@@deriving equal, globalize, sexp_of]
    end
    in
    let f_map output i =
      output [%message "" (i : int)];
      Int.to_string i
    in
    let f_bind output a b =
      output [%message "" (a : int) (b : int)];
      [ a; a * a; b; b * b ]
    in
    List.iter examples ~f:(fun example1 ->
      List.iter examples ~f:(fun example2 ->
        let result =
          let open Let_syntax in
          Test_result.with_output (fun output ->
            [%globalize: T.t]
              (let%mapl x =
                 let%bindl a = example1
                 and b = example2 in
                 f_bind output a b
               in
               f_map output x) [@nontail])
        in
        let expected =
          let open List.Let_syntax in
          Test_result.with_output (fun output ->
            [%globalize: T.t]
              (let%map x =
                 let%bind a = example1
                 and b = example2 in
                 f_bind output a b
               in
               f_map output x))
        in
        Expect_test_helpers_core.require_equal
          (module struct
            type t = T.t Test_result.t [@@deriving equal, sexp_of]
          end)
          result
          expected;
        Core.print_s ([%sexp_of: T.t Test_result.t] result)));
    [%expect
      {|
      ((result (Ok ())) (outputs ()))
      ((result (Ok ())) (outputs ()))
      ((result (Ok ())) (outputs ()))
      ((result (Ok ())) (outputs ()))
      ((result
        (Ok
         (1 1 1 1 1 1 123 15129 1 1 12 144 123 15129 1 1 123 15129 123 15129 123
          15129 12 144 12 144 1 1 12 144 123 15129 12 144 12 144)))
       (outputs
        (((a 1) (b 1)) ((a 1) (b 123)) ((a 1) (b 12)) ((a 123) (b 1))
         ((a 123) (b 123)) ((a 123) (b 12)) ((a 12) (b 1)) ((a 12) (b 123))
         ((a 12) (b 12)) (i 1) (i 1) (i 1) (i 1) (i 1) (i 1) (i 123) (i 15_129)
         (i 1) (i 1) (i 12) (i 144) (i 123) (i 15_129) (i 1) (i 1) (i 123)
         (i 15_129) (i 123) (i 15_129) (i 123) (i 15_129) (i 12) (i 144) (i 12)
         (i 144) (i 1) (i 1) (i 12) (i 144) (i 123) (i 15_129) (i 12) (i 144)
         (i 12) (i 144))))
      ((result
        (Ok
         (1 1 0 0 1 1 0 0 1 1 0 0 123 15129 0 0 123 15129 0 0 123 15129 0 0 12 144
          0 0 12 144 0 0 12 144 0 0)))
       (outputs
        (((a 1) (b 0)) ((a 1) (b 0)) ((a 1) (b 0)) ((a 123) (b 0)) ((a 123) (b 0))
         ((a 123) (b 0)) ((a 12) (b 0)) ((a 12) (b 0)) ((a 12) (b 0)) (i 1)
         (i 1) (i 0) (i 0) (i 1) (i 1) (i 0) (i 0) (i 1) (i 1) (i 0) (i 0)
         (i 123) (i 15_129) (i 0) (i 0) (i 123) (i 15_129) (i 0) (i 0) (i 123)
         (i 15_129) (i 0) (i 0) (i 12) (i 144) (i 0) (i 0) (i 12) (i 144) (i 0)
         (i 0) (i 12) (i 144) (i 0) (i 0))))
      ((result (Ok ())) (outputs ()))
      ((result
        (Ok
         (0 0 1 1 0 0 123 15129 0 0 12 144 0 0 1 1 0 0 123 15129 0 0 12 144 0 0 1 1
          0 0 123 15129 0 0 12 144)))
       (outputs
        (((a 0) (b 1)) ((a 0) (b 123)) ((a 0) (b 12)) ((a 0) (b 1)) ((a 0) (b 123))
         ((a 0) (b 12)) ((a 0) (b 1)) ((a 0) (b 123)) ((a 0) (b 12)) (i 0)
         (i 0) (i 1) (i 1) (i 0) (i 0) (i 123) (i 15_129) (i 0) (i 0) (i 12)
         (i 144) (i 0) (i 0) (i 1) (i 1) (i 0) (i 0) (i 123) (i 15_129) (i 0)
         (i 0) (i 12) (i 144) (i 0) (i 0) (i 1) (i 1) (i 0) (i 0) (i 123)
         (i 15_129) (i 0) (i 0) (i 12) (i 144))))
      ((result
        (Ok
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
       (outputs
        (((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0))
         ((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)) ((a 0) (b 0)) (i 0) (i 0)
         (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0)
         (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0)
         (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0) (i 0)
         (i 0))))
      |}]
  ;;
end
