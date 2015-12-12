(* AT&T Syntax assembler DSL *)
structure ATT = struct
    structure A = Asm
    structure I = Inst
    val genop = A.genop
    val empty = A.empty

    exception NotReg
    exception Scale

    fun intToScale 1 = A.S1
      | intToScale 2 = A.S2
      | intToScale 4 = A.S4
      | intToScale 8 = A.S8
      | intToScale _ = raise Scale

    fun intToDisp disp = let
        val w = Word32.fromInt disp
    in
        if w <= 0wxff
        then A.Disp8 (Word8.fromLarge w)
        else A.Disp32 w
    end

    fun r (A.Reg32 reg) = reg
      | r _ = raise NotReg

    fun aR           reg                = A.Addr32 (r reg, A.Disp0,        NONE)
    fun aRI         (reg, index)        = A.Addr32 (r reg, A.Disp0,        SOME(r index, intToScale 1))
    fun aRIS        (reg, index, scale) = A.Addr32 (r reg, A.Disp0,        SOME(r index, intToScale scale))
    fun aDR   (disp, reg)               = A.Addr32 (r reg, intToDisp disp, NONE)
    fun aDRI  (disp, reg, index)        = A.Addr32 (r reg, intToDisp disp, SOME(r index, intToScale 1))
    fun aDRIS (disp, reg, index, scale) = A.Addr32 (r reg, intToDisp disp, SOME(r index, intToScale scale))
    (* fun $ x = Const x *)

    fun $ x = let
        val w = Word.fromInt x
    in
        if w <= 0wxff
        then A.Imm8 (Word8.fromLarge w)
        else A.Imm32 w
    end

    val eax = A.Reg32 A.EAX
    val ecx = A.Reg32 A.ECX
    val edx = A.Reg32 A.EDX
    val ebx = A.Reg32 A.EBX
    val esp = A.Reg32 A.ESP
    val ebp = A.Reg32 A.EBP
    val esi = A.Reg32 A.ESI
    val edi = A.Reg32 A.EDI

    (* type addr32 = gpr32 * disp * (gpr32 * scale) option  *)

    (* addl eax (%eax) *)
    (* addl eax (%(eax, ebx)) *)
    (* addl eax (%(eax, ebx, 4)) *)
    (* addl eax (%(4, eax)) *)
    fun addl (op1 as A.Reg32 _)  op2 = (genop op1 op2) # {opcode = I.O1 0wx01}
      | addl (op1 as A.Addr32 _) op2 = (genop op1 op2) # {opcode = I.O1 0wx03}
      | addl (op1 as A.Imm32 _)  op2 = (genop op1 op2) # {opcode = I.O1 0wx81}
      | addl (op1 as A.Imm8 _)   op2 = (genop op1 op2) # {opcode = I.O1 0wx83}

    fun xorl (op1 as A.Reg32 _)  op2 = (genop op1 op2) # {opcode = I.O1 0wx31}
      | xorl (op1 as A.Addr32 _) op2 = (genop op1 op2) # {opcode = I.O1 0wx33}
      | xorl (op1 as A.Imm32 _)  op2 = (genop op1 op2) # {opcode = I.O1 0wx81}
      | xorl (op1 as A.Imm8 _)   op2 = let val code as {modr = modr, ...} = (genop op1 op2) # {opcode = I.O1 0wx83}
                                       in code # {modr = Option.map (fn x => x # {reg = I.R6}) modr} end

    val ret = empty # {opcode = I.O1 0wxc3}

end
