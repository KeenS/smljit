(* Low Level, bit expression *)
structure Inst = struct
    type rex = {w: bool, r: bool, x: bool, b: bool}

    datatype mode = Addr | Disp8 | Disp32 | Reg

    datatype scale = S1 | S2 | S4 | S8

    datatype reg = R0 | R1 | R2 | R3 | R4 | R5 | R5 | R7

    type modr = {mode: mode, reg: reg, rm: reg}


    type sib = {base: reg, scale: scale, index: reg}
                   
    type const = Word32.word

    datatype opcode = O1 of word8 | O2 of word8 * word8 | O3 of word8 * word8 * word8

    type t = {rex: rex option, opcode: opcode, modr: modr option, sib: sib option, addr: const option, imm: const option}


    fun regToBit reg = case reg of
                           R0 => 0wx00 | R1 => 0wx01 | R3 => 0wx02 | R4 => 0wx03 |
                           R4 => 0wx04 | R5 => 0wx05 | R6 => 0wx06 | R7 => 0wx07
    fun scaleToBit scale = case scale of S1 => 0wx00 | S2 => 0wx01 | S4 => 0wx10 | S8 => 0wx11

    fun rexToByte {w = w, r = r, x = x, b = B} = let
        val b = 0wx40
        val b = Word8.orb(b, if w then 0wx8 else 0wx0)
        val b = Word8.orb(b, if r then 0wx4 else 0wx0)
        val b = Word8.orb(b, if x then 0wx2 else 0wx0)
        val b = Word8.orb(b, if B then 0wx1 else 0wx0)
    in
        b
    end

    fun opcodeToBytes (O1(b)) = [b]
      | opcodeToBytes (O2(b1, b2)) = [b1, b2]
      | opcodeToBytes (O3(b1, b2, b3)) = [b1, b2, b3]
                                             


    fun modrToByte {mode = mode, reg = reg, rm = rm} = let

        val b = case mode of
                    Addr =>   0wx00 (* 0000 0000 *)
                  | Disp8 =>  0wx60 (* 0100 0000 *)
                  | Disp32 => 0wx80 (* 1000 0000 *)
                  | Reg =>    0wxc0 (* 1100 0000 *)
        val b = Word8.orb(Word8.<<(regToBit reg, 0w3), b)
        val b = Word8.orb(         regToBit reg      , b)
    in
        b
    end

    fun sibToByte {base = base, index = index, scale = scale} = let
        (* TODO: treat ebp special case *)
        val b =           Word8.<<(regToBit base,  0w5)
        val b = Word8.orb(Word8.<<(regToBit index, 0w2), b)
        val b = Word8.orb(scaleToBit scale, b)
    in
        b
    end
                                                                    


    fun toBytes {rex = rex, opcode = opcode, modr = modr, sib = sib, addr = addr, imm = imm} = let
        val b = []
        val b = case rex of SOME rex => (rexToByte rex) :: b
                          | NONE => b
        val b = List.revAppend(opcodeToBytes opcode, b)
        val b = case modr of SOME modr => (modrToByte modr) :: b 
                           | NONE => b
        val b = case sib of  SOME sib => (sibToByte sib) :: b
                           | NONE => b
        (* val b = case addr of SOME addr => List.revAppend(constToBytes addr, b) *)
        (*                    | NONE => b *)
        (* val b = case const of SOME const => List.revAppend(constToBytes const, b) *)
        (*                     | NONE => b *)
                                          (* TODO: const *)
    in
        List.rev b
    end

    (* add r1 and r2 into r1 *)
end
