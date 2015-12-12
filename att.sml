(* AT&T Syntax assembler DSL *)
structure ATT = struct

    exception NotReg

    fun a1 reg                         = (reg, 0   , NONE)
    fun a2 (disp, reg)                 = (reg, disp, NONE)
    fun a3 (disp, (reg, index))        = (reg, disp, SOME(index, S1))
    fun a3 (disp, (reg, index, scale)) = (reg, disp, SOME(index, scale))
    fun $ x = Const x

    val eax = EAX
    val ecx = ECX
    val edx = EDX
    val ebx = EBX
    val esp = ESP
    val ebp = EBP
    val esi = ESI
    val edi = EDI

    (* type addr32 = gpr32 * disp * (gpr32 * scale) option  *)

    (* addl eax (%(eax))*)
    fun addl op1 op2 =


end
