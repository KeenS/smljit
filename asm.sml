structure Asm = struct
    structure I = Inst

    (* general purpose registers *)
    (* datatype gpr64 = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15 *)
    datatype gpr32 =  EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI (* | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D *)
    datatype gpr16 =  AX  |  CX |  DX |  BX |  SP |  BP |  SI |  DI (* | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W *)
    datatype gpr8  =  AL  |  CL |  DL |  BL | (* SPL | BPL | SIL | DIL | R8B | R8B | R10B | R11B | R12B | R13B | R14B | R15B *)
                      AH  |  CH |  DH |  BH
    datatype gpr = GPR8 of gpr8 | GPR16 of gpr16 | GPR32 of gpr32 (* | GPR64 of gpr64 *)

    datatype disp = Disp0 | Disp8 of Word8.word | Disp32 of Word32.word
    datatype scale = datatype I.scale
    type addr32 = gpr32 * disp * (gpr32 * scale) option 
    (* sib in Inst is redundant *)
    (* type sib = scale * gpr32 * gpr32 *)
    (* datatype rm32 = Reg32 of gpr32 | Addr32 of addr32 | Sib of sib *)

    datatype operand32 = Reg32 of gpr32 | Addr32 of addr32 | Imm32 of Word32.word

    fun gpr32ToReg gpr32 = case gpr32 of
                               EAX => I.R0 | ECX => I.R1 | EDX => I.R2 | EBX => I.R3 |
                               ESP => I.R4 | EBP => I.R5 | ESI => I.R6 | EDI => I.R7
    fun gpr16ToReg gpr16 = case gpr16 of
                               AX => I.R0 | CX => I.R1 | DX => I.R2 | BX => I.R3 |
                               SP => I.R4 | BP => I.R5 | SI => I.R6 | DI => I.R7

    fun gpr8ToReg gpr8 = case gpr8 of
                              AL => I.R0 | CL => I.R1 | DL => I.R2 | BL => I.R3 |
                              AH => I.R0 | CH => I.R1 | DH => I.R2 | BH => I.R3


    fun word32ToTuple w = let
        val w1 =           Word32.andb(w,         0wxff)
        val w2 = Word32.andb(Word32.>>(w, 0wx8),  0wxff)
        val w3 = Word32.andb(Word32.>>(w, 0wx16), 0wxff)
        val w4 = Word32.andb(Word32.>>(w, 0wx24), 0wxff)
    in
        (Word8.fromLarge w1, Word8.fromLarge w2, Word8.fromLarge w3, Word8.fromLarge w4)
    end

    fun dispToConst disp = case disp of
                               Disp0 => I.C0
                             | Disp8 w => I.C1(w)
                             | Disp32 w => I.C4(word32ToTuple w)

    
    fun dispToMode disp = case disp of
                               Disp0 => I.Addr
                             | Disp8 _ => I.Disp8
                             | Disp32 _ => I.Disp32

    val immToConst = I.C4 o word32ToTuple


    (* Backend for DSL of AT&T and Intel. This itself is AT&T style.  *)
    structure Backend = struct
        val empty = {rex = NONE, opcode = I.O1 0wx00, modr = NONE, sib = NONE, addr = I.C0, imm = I.C0}
        exception InstFormat

        (* code %op1, %op2 *)
        fun genop (Reg32 op1) (Reg32 op2): I.t =
          empty # {modr = SOME {mode = I.Reg, reg = gpr32ToReg op1, rm = gpr32ToReg op2}}
          (* code %op1, disp(%op2) *)
          | genop (Reg32 op1) (Addr32 (op2, disp, NONE)) =
            empty # {modr = SOME {mode = dispToMode disp, reg = gpr32ToReg op1, rm = gpr32ToReg op2},
                     addr = dispToConst disp}
          (* code %op1 disp(%base, %index, scale) *)
          | genop (Reg32 op1) (Addr32 (base, disp, SOME(index, scale))) =
            empty # {modr = SOME {mode = I.Reg, reg = gpr32ToReg op1, rm = I.R4},
                     sib = SOME {scale = scale, index = gpr32ToReg index, base = gpr32ToReg base},
                     addr = dispToConst disp}

          (* code imm, %op2 *)
          | genop (Imm32 imm) (Reg32 op2) =
            empty # {modr = SOME {mode = I.Reg, reg = I.R0, rm = gpr32ToReg op2},
                     imm =  immToConst imm}
          (* code imm, disp(%op2) *)
          | genop (Imm32 imm) (Addr32 (op2, disp, NONE)) =
            empty # {modr = SOME {mode = dispToMode disp, reg = I.R0, rm = gpr32ToReg op2},
                     addr = dispToConst disp, imm = immToConst imm}
          (* code imm, disp(%base, %index, scale) *)
          | genop (Imm32 imm) (Addr32 (base, disp, SOME(index, scale))) =
            empty # {modr = SOME {mode = dispToMode disp, reg = I.R0, rm = I.R4},
                     sib = SOME {scale = scale, index = gpr32ToReg index, base = gpr32ToReg base},
                     addr = dispToConst disp, imm = immToConst imm}

          (** in these cases, op1 and op2 will be flipped. Confusing *)
          (* code disp(%op1), %op2 *)
          | genop (Addr32 (op1, disp, NONE)) (Reg32 op2) =
            empty # {modr = SOME {mode = dispToMode disp, reg = gpr32ToReg op2, rm = gpr32ToReg op1}}
          (* code disp(%base, %index, scale), %op2 *)
          | genop (Addr32 (base, disp, SOME(index, scale))) (Reg32 op2) =
            empty # {modr = SOME {mode = dispToMode disp, reg = gpr32ToReg op2, rm = I.R4},
                     sib = SOME {scale = scale, index = gpr32ToReg index, base = gpr32ToReg base},
                     addr = dispToConst disp}
          | genop (Addr32 _) _ = raise InstFormat


          | genop _ (Imm32 _) = raise InstFormat

    end
end

