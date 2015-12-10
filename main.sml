
fun run () = let
    val freturn1 = Emit.fromMachineCode Emit.return1 :_import () -> int
    val () = print ((Int.toString (freturn1 ())) ^ "\n")
    val fadd1 = Emit.fromMachineCode Emit.add1 :_import (int) -> int
    val () = print ((Int.toString (fadd1 3)) ^ "\n")
    val fadd = Emit.fromMachineCode Emit.add :_import (int, int) -> int
    val () = print ((Int.toString (fadd (3, 8))) ^ "\n")
in
    ()
end

val () = run ()
