(* AT&T Syntax assembler DSL *)
structure ATT = struct
    structure A = Asm
    structure I = Inst
    val genop = A.genop
    val empty = A.empty
    val imm32ToConst = A.imm32ToConst
    val op <~ = A.setReg
    infix 5 <~

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

    fun regToWord8 A.EAX = 0wx00
      | regToWord8 A.ECX = 0wx01
      | regToWord8 A.EDX = 0wx02
      | regToWord8 A.EBX = 0wx03
      | regToWord8 A.ESP = 0wx04
      | regToWord8 A.EBP = 0wx05
      | regToWord8 A.ESI = 0wx06
      | regToWord8 A.EDI = 0wx07
    (* type addr32 = gpr32 * disp * (gpr32 * scale) option  *)

    (* addl eax (&eax) *)
    (* addl eax (&(eax, ebx)) *)
    (* addl eax (&(eax, ebx, 4)) *)
    (* addl eax (&(4, eax)) *)
    fun gop  opcode op1 op2 = (genop op1 op2) # {opcode = opcode}

    fun addl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx01) op1 op2 
      | addl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx03) op1 op2 
      | addl (A.Imm32 op1)  (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                       opcode = I.O1 (0wx05)}
      | addl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 
      | addl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 

    fun adcl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx11) op1 op2
      | adcl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx13) op1 op2
      | adcl (A.Imm32 op1)  (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                        opcode = I.O1 (0wx15)}
      | adcl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R2
      | adcl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R2

    fun subl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx29) op1 op2
      | subl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx2B) op1 op2
      | subl (A.Imm32 op1)  (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                        opcode = I.O1 (0wx2d)}
      | subl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R5
      | subl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R5

    fun sbbl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx19) op1 op2
      | sbbl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx1B) op1 op2
      | sbbl (A.Imm32 op1)  (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                        opcode = I.O1 (0wx1d)}
      | sbbl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R3
      | sbbl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R3

    fun incl (A.Reg32 r)  = empty # {opcode = I.O1 (0wx40 + (regToWord8 r))}
      | incl (op1 as A.Addr32 _) = gop (I.O1 0wxFF) eax op1 <~ I.R0
      | incl _ = raise NotReg

    fun decl (A.Reg32 r)  = empty # {opcode = I.O1 (0wx48 + (regToWord8 r))}
      | decl (op1 as A.Addr32 _) = gop (I.O1 0wxFF) eax op1 <~ I.R1
      | decl _ = raise NotReg
                                            
    fun negl (op1 as A.Reg32 _)  = gop (I.O1 0wxF7) eax op1 <~ I.R3
      | negl (op1 as A.Addr32 _) = gop (I.O1 0wxF7) eax op1 <~ I.R3
      | negl _ = raise NotReg


    fun andl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx21) op1 op2
      | andl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx23) op1 op2
      | andl (A.Imm32 op1) (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                      opcode = I.O1 (0wx25)}
      | andl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R4
      | andl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R4

    fun orl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx09) op1 op2
      | orl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx0b) op1 op2
      | orl (A.Imm32 op1) (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                     opcode = I.O1 (0wx0d)}
      | orl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R1
      | orl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R1

    fun xorl (op1 as A.Reg32 _)  op2 = gop (I.O1 0wx31) op1 op2
      | xorl (op1 as A.Addr32 _) op2 = gop (I.O1 0wx33) op1 op2
      | xorl (A.Imm32 op1) (A.Reg32 A.EAX) = empty # {imm = imm32ToConst op1,
                                                      opcode = I.O1 (0wx35)}
      | xorl (op1 as A.Imm32 _)  op2 = gop (I.O1 0wx81) op1 op2 <~ I.R6
      | xorl (op1 as A.Imm8 _)   op2 = gop (I.O1 0wx83) op1 op2 <~ I.R6

    fun notl (op1 as A.Reg32 _)  = gop (I.O1 0wxF7) eax op1 <~ I.R2
      | notl (op1 as A.Addr32 _) = gop (I.O1 0wxF7) eax op1 <~ I.R2
      | notl _ = raise NotReg

    fun mull (op1 as A.Reg32 _)  = gop (I.O1 0wxF7) eax op1 <~ I.R4
      | mull (op1 as A.Addr32 _) = gop (I.O1 0wxF7) eax op1 <~ I.R4
      | mull _ = raise NotReg

    fun imull1 (op1 as A.Reg32 _)  = gop (I.O1 0wxF7) eax op1 <~ I.R5
      | imull1 (op1 as A.Addr32 _) = gop (I.O1 0wxF7) eax op1 <~ I.R5
      | imull1 _ = raise NotReg

    fun imull2 (op1 as A.Reg32 _)  (op2 as A.Reg32 _) = gop (I.O2 (0wx0f, 0wxaf)) op2 op1
      | imull2 (op1 as A.Addr32 _) (op2 as A.Reg32 _) = gop (I.O2 (0wx0f, 0wxaf)) op1 op2
      | imull2 (op1 as A.Imm32 _)  (op2 as A.Reg32 _) = gop (I.O1 0wx81) op1 op2 <~ I.R6
      | imull2 (op1 as A.Imm8 _)   (op2 as A.Reg32 _) = gop (I.O1 0wx83) op1 op2 <~ I.R6
      | imull2 _  _ =  raise NotReg



    val ret = empty # {opcode = I.O1 0wxc3}

end
