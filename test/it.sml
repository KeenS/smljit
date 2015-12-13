structure IntegrationTest = struct
    open SMLUnit
    open Assert
    open ATT
    val asm = Emit.fromInstsToBytes
    val eq = assertEqualList assertEqualWord8
    fun suite () = Test.labelTests [
            (* 

8:  01 c8                   add    eax,ecx
a:  03 01                   add    eax,DWORD PTR [ecx]
c:  03 41 04                add    eax,DWORD PTR [ecx+0x4]
f:  03 44 19 04             add    eax,DWORD PTR [ecx+ebx*1+0x4]
13: 03 44 4b 04             add    eax,DWORD PTR [ebx+ecx*2+0x4]
17: 03 44 8b 04             add    eax,DWORD PTR [ebx+ecx*4+0x4]
1b: 03 44 cb 04             add    eax,DWORD PTR [ebx+ecx*8+0x4] 
            *)
            (* 0:  83 c0 01                add    eax,0x1 *)
            ("addl $0x1, %eax", fn () => eq [0wx83, 0wxc0, 0wx01] (asm [addl ($0x1) eax])),
            (* 3:  05 00 04 00 00          add    eax,0x400 *)
            ("addl $0x400, %eax", fn () => eq [0wx05, 0wx00, 0wx04, 0wx00, 0wx00] (asm [addl ($0x400) eax]))
        ]
end
