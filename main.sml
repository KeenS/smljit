open ATT

fun println x = print (x ^ "\n")
val printInst = println o String.concatWith " " o List.map (fn x => "0x" ^ (Word8.toString x))
val return1  =
    (* 0:  b8 01 00 00 00          mov    eax,0x1  *)
    [
      0wxb8, 0wx01, 0wx00, 0wx00, 0wx00
    ] 



val add1 = 
    (* 0:  8b 44 24 04             mov    eax,DWORD PTR [esp+0x4] *)
    (* 4:  83 c0 01                add    eax,0x1 *)
    [
      0wx8b, 0wx44, 0wx24, 0wx04,
      0wx83, 0wxc0, 0wx01
    ]

val add  = 
    (* 0:  8b 44 24 04             mov    eax,DWORD PTR [esp+0x4] *)
    (* 4:  8b 4c 24 08             mov    ecx,DWORD PTR [esp+0x8] *)
    (* 8:  01 c8                   add    eax,ecx *)
    [
      0wx8b, 0wx44, 0wx24, 0wx04,
      0wx8b, 0wx4c, 0wx24, 0wx08,
      0wx01, 0wxc8
    ]

fun run () = let
    val freturn1 = Emit.fromMachineCode return1 :_import () -> int
    val () = println (Int.toString (freturn1 ()))
    val fadd1 = Emit.fromMachineCode add1 :_import (int) -> int
    val () = println (Int.toString (fadd1 3))
    val fadd = Emit.fromMachineCode add :_import (int, int) -> int
    val () = println (Int.toString (fadd (3, 8)))
    val () = print "addl eax ebx: "
    val () = printInst (Inst.toBytes (addl eax ebx))
    (* val () = print "addl (%eax) eax: " *)
    (* val () = printInst (Inst.toBytes (addl (%eax) eax)) *)
    val () = print "addl ($1) eax: "
    val () = printInst (Inst.toBytes (addl ($1) eax))
    val () = print "xorl eax eax, addl ($1 eax): "
    val () = List.app (printInst o Inst.toBytes) [
            xorl eax eax,
            addl ($1) eax,
            ret]
    val freturn1' = Emit.fromInsts [
            xorl eax eax,
            addl ($1) eax,
            ret]:_import () -> int
    val () = println (Int.toString (freturn1' ()))

in
    ()
end

val () = run ()
