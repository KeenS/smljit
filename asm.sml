(* AT&T Syntax assembler *)
structure Asm = struct
    structure I = Inst
    (* general purpose registers *)

    (* datatype gpr64 = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15 *)
    datatype gpr32 =  EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI (* | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D *)
    datatype gpr16 =  AX  |  CX |  DX |  BX |  SP |  BP |  SI |  DI (* | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W *)
    datatype gpr8  =  AL  |  CL |  DL |  BL | (* SPL | BPL | SIL | DIL | R8B | R8B | R10B | R11B | R12B | R13B | R14B | R15B *)
                      AH  |  CH |  DH |  BH
    datatype gpr = GPR8 of gpr8 | GPR16 of gpr16 | GPR32 of gpr32 (* | GPR64 of gpr64 *)

    fun gpr32ToReg gpr32 = case gpr32 of
                               EAX => I.R0 | ECX => I.R1 | EDX => I.R2 | EBX => I.R3 |
                               ESP => I.R4 | EBP => I.R5 | ESI => I.R6 | EDI => I.R7
    fun gpr16ToReg gpr16 = case gpr16 of
                               AX => I.R0 | CX => I.R1 | DX => I.R2 | BX => I.R3 |
                               SP => I.R4 | BP => I.R5 | SI => I.R6 | DI => I.R7

    fun gpr8ToReg gpr8 = case gpr8 of
                              AL => I.R0 | CL => I.R1 | DL => I.R2 | BL => I.R3 |
                              AH => I.R0 | CH => I.R1 | DH => I.R2 | BH => I.R3


    fun addl op1 op2 = {rex = NONE, opcode = O1(0wx01), modr = SOME {mode = Reg, reg = gpr32ToReg op1, rm = op2}, sib = NONE, addr = NONE, imm = NONE}


end

