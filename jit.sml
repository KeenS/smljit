structure JIT = struct
val PAGE_SIZE = 0w4096
val posix_memalign = _import "posix_memalign": (unit ptr ref, word, word) -> int
val mprotect = _import "mprotect": (unit ptr, word, word) -> int
val memset = _import "memset": (unit ptr, word, word) -> unit ptr
val free = _import "free": unit ptr -> ()
val printf = _import "printf": (string, unit ptr) -> ()

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

val PROT_RWEX = 0wx7


type jitptr = unit ptr
fun jitMemory size: jitptr = let
    val msize = size * PAGE_SIZE
    val pageRef: unit ptr ref = ref (Pointer.NULL ())
    val _ = posix_memalign (pageRef, PAGE_SIZE, msize)
    val page = !pageRef
    val () = if Pointer.isNull page
             then print "null\n"
             else ()
    val _ = mprotect (page, msize, PROT_RWEX)
                   
    (* init with ret for safety *)
    val _ = memset (page, 0wxc3, msize)
in
    page
end

fun freeJit jitMem = free (SMLSharp_Builtin.Pointer.toUnitPtr jitMem)


fun writeReturn1 (page: jitptr) = let
    val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
    val store = SMLSharp_Builtin.Pointer.store
    val advance = SMLSharp_Builtin.Pointer.advance
    val page: word8 ptr = fromUnitPtr page
    fun pushWord page (word: word8) = (store (page, word); advance (page, 1))
    (* 0:  b8 01 00 00 00          mov    eax,0x1  *)
    val page = pushWord page 0wxb8
    val page = pushWord page 0wx01
    val page = pushWord page 0wx00
    val page = pushWord page 0wx00
    val _    = pushWord page 0wx00

in
    ()
end

fun run () = let
    val import = SMLSharp_Builtin.Pointer.toCodeptr

    val jit = jitMemory 0w1
    val _ = writeReturn1 jit
    val return1 = import jit :_import () -> int
    val x = return1 ()
    val print = print ((Int.toString x) ^ "\n")
    val () = free jit
in
    ()
end
end
