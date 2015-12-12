structure Emit = struct
    val posix_memalign = _import "posix_memalign": (unit ptr ref, word, word) -> int
    val mprotect = _import "mprotect": (unit ptr, word, word) -> int
    val memset = _import "memset": (unit ptr, word, word) -> unit ptr
    val free = _import "free": unit ptr -> ()

    (* 
#define PROT_READ	0x1		/* Page can be read.  */
#define PROT_WRITE	0x2		/* Page can be written.  */
#define PROT_EXEC	0x4		/* Page can be executed.  */
#define PROT_NONE	0x0		/* Page can not be accessed.  */
    *)

    val PROT_READ  = 0wx1
    val PROT_WRITE = 0wx2
    val PROT_EXEC  = 0wx4
    val PROT_NONE  = 0wx0

    val PAGE_SIZE = 0w4096

    type jitptr = word8 ptr
    val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
    val toUnitPtr = SMLSharp_Builtin.Pointer.toUnitPtr
    val toCodeptr = SMLSharp_Builtin.Pointer.toCodeptr
    val store = Pointer.store
    val advance = Pointer.advance


    fun jitMemory size: jitptr = let
        val op orb = Word.orb
        infix 5 orb
        val msize = size * PAGE_SIZE
        val pageRef: unit ptr ref = ref (Pointer.NULL ())
        val ret = posix_memalign (pageRef, PAGE_SIZE, msize)
        val page = if ret = 0
                   then !pageRef
                   else raise Fail "memory allocation failed"
        val PROT_RWEX = PROT_READ orb PROT_WRITE orb PROT_EXEC
        val ret = mprotect (page, msize, PROT_RWEX)
        val () = if ret = 0
                 then ()
                 else raise Fail "memory protection error"
        (* init with ret for safety *)
        val _ = memset (page, 0wxc3, msize)
    in
        fromUnitPtr page
    end

    fun freeJit (jitMem: jitptr) = free (SMLSharp_Builtin.Pointer.toUnitPtr jitMem)

    fun pushWord page word = (store (page, word); advance (page, 1))
    fun pushWords (page: jitptr) l = List.foldl (fn(w,page) => pushWord page w) page l

    val import: jitptr -> codeptr = toCodeptr o toUnitPtr

    fun fromMachineCode l = let
        val len = Word.fromInt(List.length l)
        val size = (len + PAGE_SIZE) div PAGE_SIZE
        val page = jitMemory size
        val _ = pushWords page l
    in
        import page
    end

    val fromInsts = fromMachineCode o List.rev o List.foldl (fn (i, acc) => List.revAppend(Inst.toBytes i ,acc)) []
end
