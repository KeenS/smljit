structure IntegrationTest = struct
    open SMLUnit
    open Assert
    open ATT
    val asm = Emit.fromInstsToBytes
    val eq = assertEqualList assertEqualWord8
    (* One operands template *)
    (* not eax *)
    (* not eax *)
    (* not DWORD PTR[eax] *)
    (* not DWORD PTR[eax+1] *)
    (* not DWORD PTR[eax+0x100] *)
    (* not DWORD PTR[eax*2+edx+1] *)
    (* not DWORD PTR[eax*2+edx+100] *)
    (* Two operands template *)
    (* add eax, 0x1 *)
    (* add eax, 0x100 *)
    (* add ecx, 0x100 *)
    (* add eax, ecx *)
    (* add eax, DWORD PTR[ecx] *)
    (* add eax, DWORD PTR[ecx+1] *)
    (* add eax, DWORD PTR[ecx+0x100] *)
    (* add eax, DWORD PTR[ecx*2+edx+1] *)
    (* add eax, DWORD PTR[ecx*2+edx+100] *)
    (* add DWORD PTR[eax], ecx *)
    (* add DWORD PTR[eax+1], ecx *)
    (* add DWORD PTR[eax+0x100], ecx *)
    (* add DWORD PTR[eax*2+edx+1], ecx *)
    (* add DWORD PTR[eax*2+edx+100], ecx *)


    fun suite () = Test.labelTests [
          ("addl $0x1, %eax",
           (* 0:  83 c0 01                add    eax,0x1 *)
           fn () => eq [0wx83, 0wxc0, 0wx01] (asm [addl ($0x1) eax])),
          ("addl $0x100, %eax",
           (* 3:  05 00 01 00 00          add    eax,0x100 *)
           fn () => eq [0wx05, 0wx00, 0wx01, 0wx00, 0wx00] (asm [addl ($0x100) eax])),
          ("addl $0x100, %ecx",
           (* 8:  81 c1 00 01 00 00       add    ecx,0x100 *)
           fn () => eq [0wx81, 0wxc1, 0wx00, 0wx01, 0wx00, 0wx00] (asm [addl ($0x100) ecx])),
          ("addl %ecx, %eax",
           (* e:  01 c8                   add    eax,ecx *)
           fn () => eq [0wx01, 0wxc8] (asm [addl ecx eax])),
          ("addl (%ecx), %eax",
           (* 10: 03 01                   add    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx03, 0wx01] (asm [addl (&ecx) eax])),
          (* 12: 03 41 01                add    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 03 81 00 01 00 00       add    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 03 44 4a 01             add    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 03 44 4a 64             add    eax,DWORD PTR [edx+ecx*2+0x64] *)          
          ("addl %ecx, (%eax)",
           (* 23: 01 08                   add    DWORD PTR [eax],ecx *)
           fn () => eq [0wx01, 0wx08] (asm [addl ecx (&eax)])),
          (* 25: 01 48 01                add    DWORD PTR [eax+0x1],ecx *)
          (* 28: 01 88 00 01 00 00       add    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 01 4c 42 01             add    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 01 4c 42 64             add    DWORD PTR [edx+eax*2+0x64],ecx  *)


          ("adcl $0x1, %eax",
           (* 0:  83 d0 01                adc    eax,0x1 *)
           fn () => eq [0wx83, 0wxd0, 0wx01] (asm [adcl ($0x1) eax])),
          ("adcl $0x100, %eax",
           (* 3:  15 00 01 00 00          adc    eax,0x100 *)
           fn () => eq [0wx15, 0wx00, 0wx01, 0wx00, 0wx00] (asm [adcl ($0x100) eax])),
          ("adcl $0x100, %ecx",
           (* 8:  81 d1 00 01 00 00       adc    ecx,0x100 *)
           fn () => eq [0wx81, 0wxd1, 0wx00, 0wx01, 0wx00, 0wx00] (asm [adcl ($0x100) ecx])),
          ("adcl %ecx, %eax",
           (* e:  11 c8                   adc    eax,ecx *)
           fn () => eq [0wx11, 0wxc8] (asm [adcl ecx eax])),
          ("adcl (%ecx) %eax",
           (* 10: 13 01                   adc    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx13, 0wx01] (asm [adcl (&ecx) eax])),
          (* 12: 13 41 01                adc    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 13 81 00 01 00 00       adc    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 13 44 4a 01             adc    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 13 44 4a 64             adc    eax,DWORD PTR [edx+ecx*2+0x64] *)
          ("adcl %ecx, (%eax)",
           (* 23: 11 08                   adc    DWORD PTR [eax],ecx *)
           fn () => eq [0wx11, 0wx08] (asm [adcl ecx (&eax)])),
          (* 25: 11 48 01                adc    DWORD PTR [eax+0x1],ecx *)
          (* 28: 11 88 00 01 00 00       adc    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 11 4c 42 01             adc    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 11 4c 42 64             adc    DWORD PTR [edx+eax*2+0x64],ecx  *)


          ("subl $0x1, %eax",
           (* 0:  83 e8 01                sub    eax,0x1 *)
           fn () => eq [0wx83, 0wxe8, 0wx01] (asm [subl ($0x1) eax])),
          ("subl $0x100, %eax",
           (* 3:  2d 00 01 00 00          sub    eax,0x100 *)
           fn () => eq [0wx2d, 0wx00, 0wx01, 0wx00, 0wx00] (asm [subl ($0x100) eax])),
          ("subl $0x100, %ecx",
          (* 8:  81 e9 00 01 00 00       sub    ecx,0x100 *)
           fn () => eq [0wx81, 0wxe9, 0wx00, 0wx01, 0wx00, 0wx00] (asm [subl ($0x100) ecx])),
          ("subl %ecx, %eax",
           (* e:  29 c8                   sub    eax,ecx *)
           fn () => eq [0wx29, 0wxc8] (asm [subl ecx eax])),
          ("subl (%ecx) %eax",
           (* 10: 2b 01                   sub    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx2b, 0wx01] (asm [subl (&ecx) eax])),
          (* 12: 2b 41 01                sub    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 2b 81 00 01 00 00       sub    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 2b 44 4a 01             sub    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 2b 44 4a 64             sub    eax,DWORD PTR [edx+ecx*2+0x64] *)
          ("subl %ecx (%eax)",
           (* 23: 29 08                   sub    DWORD PTR [eax],ecx *)
           fn () => eq [0wx29, 0wx08] (asm [subl ecx (&eax)])),
          (* 25: 29 48 01                sub    DWORD PTR [eax+0x1],ecx *)
          (* 28: 29 88 00 01 00 00       sub    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 29 4c 42 01             sub    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 29 4c 42 64             sub    DWORD PTR [edx+eax*2+0x64],ecx  *)



          ("sbbl $0x1, %eax",
           (* 0:  83 d8 01                sbb    eax,0x1 *)
           fn () => eq [0wx83, 0wxd8, 0wx01] (asm [sbbl ($0x1) eax])),
          ("sbbl $0x100, %eax",
           (* 3:  1d 00 01 00 00          sbb    eax,0x100 *)
           fn () => eq [0wx1d, 0wx00, 0wx01, 0wx00, 0wx00] (asm [sbbl ($0x100) eax])),
          ("sbbl $0x100, %ecx",
           (* 8:  81 d9 00 01 00 00       sbb    ecx,0x100 *)
           fn () => eq [0wx81, 0wxd9, 0wx00, 0wx01, 0wx00, 0wx00] (asm [sbbl ($0x100) ecx])),

          ("sbbl %ecx, %eax",
           (* e:  19 c8                   sbb    eax,ecx *)
           fn () => eq [0wx19, 0wxc8] (asm [sbbl ecx eax])),
          ("sbbl (%ecx) %eax",
           (* 10: 1b 01                   sbb    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx1b, 0wx01] (asm [sbbl (&ecx) eax])),          
          (* 12: 1b 41 01                sbb    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 1b 81 00 01 00 00       sbb    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 1b 44 4a 01             sbb    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 1b 44 4a 64             sbb    eax,DWORD PTR [edx+ecx*2+0x64] *)

          ("sbbl %ecx (%eax)",
           (* 23: 19 08                   sbb    DWORD PTR [eax],ecx *)
           fn () => eq [0wx19, 0wx08] (asm [sbbl ecx (&eax)])),          
          (* 25: 19 48 01                sbb    DWORD PTR [eax+0x1],ecx *)
          (* 28: 19 88 00 01 00 00       sbb    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 19 4c 42 01             sbb    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 19 4c 42 64             sbb    DWORD PTR [edx+eax*2+0x64],ecx  *)


          ("incl %eax",
           (* 0:  40                      inc    eax *)
           fn () => eq [0wx40] (asm [incl eax])),
          ("incl (%eax)",
           (* 1:  ff 00                   inc    DWORD PTR [eax] *)
           fn () => eq [0wxff, 0wx00] (asm [incl (&eax)])),
          (* 3:  ff 40 04                inc    DWORD PTR [eax+0x4] *)
          (* 6:  ff 80 00 01 00 00       inc    DWORD PTR [eax+0x100] *)
          (* c:  ff 44 41 04             inc    DWORD PTR [ecx+eax*2+0x4]  *)

          ("decl %eax",
           (* 0:  48                      dec    eax *)
           fn () => eq [0wx48] (asm [decl eax])),
          ("decl (%eax)",
           (* 1:  ff 08                   dec    DWORD PTR [eax] *)
           fn () => eq [0wxff, 0wx08] (asm [decl (&eax)])),
          (* 3:  ff 48 04                dec    DWORD PTR [eax+0x4] *)
          (* 6:  ff 88 00 01 00 00       dec    DWORD PTR [eax+0x100] *)
          (* c:  ff 4c 41 04             dec    DWORD PTR [ecx+eax*2+0x4]  *)

          ("negl %eax",
           (* 0:  f7 d8                   neg    eax *)
           fn () => eq [0wxf7, 0wxd8] (asm [negl eax])),
          ("negl (%eax)",
           (* 2:  f7 18                   neg    DWORD PTR [eax] *)
           fn () => eq [0wxf7, 0wx18] (asm [negl (&eax)])),
          (* 4:  f7 58 01                neg    DWORD PTR [eax+0x1] *)
          (* 7:  f7 98 00 01 00 00       neg    DWORD PTR [eax+0x100] *)
          (* d:  f7 5c 41 04             neg    DWORD PTR [ecx+eax*2+0x4]  *)


          ("andl $0x1, %eax",
           (* 0:  83 e0 01                and    eax,0x1 *)
           fn () => eq [0wx83, 0wxe0, 0wx01] (asm [andl ($1) eax])),
          ("andl $0x100, %eax",
           (* 3:  25 00 01 00 00          and    eax,0x100 *)
           fn () => eq [0wx25, 0wx00, 0wx01, 0wx00, 0wx00] (asm [andl ($0x100) eax])),
          ("andl $0x100, %eax",
           (* 8:  81 e1 00 01 00 00       and    ecx,0x100 *)
           fn () => eq [0wx81, 0wxe1, 0wx00, 0wx01, 0wx00, 0wx00] (asm [andl ($0x100) ecx])),
          ("andl %ecx, %eax",
           (* e:  21 c8                   and    eax,ecx *)
           fn () => eq [0wx21, 0wxc8] (asm [andl ecx eax])),
          ("andl (%ecx), %eax",
           (* 10: 23 01                   and    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx23, 0wx01] (asm [andl (&ecx) eax])),
          (* 12: 23 41 01                and    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 23 81 00 01 00 00       and    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 23 44 4a 01             and    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 23 44 4a 64             and    eax,DWORD PTR [edx+ecx*2+0x64] *)
          ("andl %ecx, (%eax)",
           (* 23: 21 08                   and    DWORD PTR [eax],ecx *)
           fn () => eq [0wx21, 0wx08] (asm [andl ecx (&eax)])),
          (* 25: 21 48 01                and    DWORD PTR [eax+0x1],ecx *)
          (* 28: 21 88 00 01 00 00       and    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 21 4c 42 01             and    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 21 4c 42 64             and    DWORD PTR [edx+eax*2+0x64],ecx *)

          ("orl $0x1, %eax",
           (* 0:  83 c8 01                or     eax,0x1 *)
           fn () => eq [0wx83, 0wxc8, 0wx01] (asm [orl ($1) eax])),
          ("orl $0x100, %eax",
           (* 3:  0d 00 01 00 00          or     eax,0x100 *)
           fn () => eq [0wx0d, 0wx00, 0wx01, 0wx00, 0wx00] (asm [orl ($0x100) eax])),
          ("orl $0x100, %eax",
           (* 8:  81 c9 00 01 00 00       or     ecx,0x100 *)
           fn () => eq [0wx81, 0wxc9, 0wx00, 0wx01, 0wx00, 0wx00] (asm [orl ($0x100) ecx])),
          ("orl %ecx, %eax",
           (* e:  09 c8                   or     eax,ecx *)
           fn () => eq [0wx09, 0wxc8] (asm [orl ecx eax])),
          ("orl (%ecx), %eax",
           (* 10: 0b 01                   or     eax,DWORD PTR [ecx] *)
           fn () => eq [0wx0b, 0wx01] (asm [orl (&ecx) eax])),
          (* 12: 0b 41 01                or     eax,DWORD PTR [ecx+0x1] *)
          (* 15: 0b 81 00 01 00 00       or     eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 0b 44 4a 01             or     eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 0b 44 4a 64             or     eax,DWORD PTR [edx+ecx*2+0x64] *)

          ("orl %ecx, (%eax)",
           (* 23: 09 08                   or     DWORD PTR [eax],ecx *)
           fn () => eq [0wx09, 0wx08] (asm [orl ecx (&eax)])),
          (* 25: 09 48 01                or     DWORD PTR [eax+0x1],ecx *)
          (* 28: 09 88 00 01 00 00       or     DWORD PTR [eax+0x100],ecx *)
          (* 2e: 09 4c 42 01             or     DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 09 4c 42 64             or     DWORD PTR [edx+eax*2+0x64],ecx  *)


          ("xorl $0x1, %eax",
           (* 0:  83 f0 01                xor    eax,0x1 *)
           fn () => eq [0wx83, 0wxf0, 0wx01] (asm [xorl ($1) eax])),
          ("xorl $0x100, %eax",
           (* 3:  35 00 01 00 00          xor    eax,0x100 *)
           fn () => eq [0wx35, 0wx00, 0wx01, 0wx00, 0wx00] (asm [xorl ($0x100) eax])),
          ("xorl $0x100, %ecx",
           (* 8:  81 f1 00 01 00 00       xor    ecx,0x100 *)
           fn () => eq [0wx81, 0wxf1, 0wx00, 0wx01, 0wx00, 0wx00] (asm [xorl ($0x100) ecx])),
          ("xorl %ecx, %eax",
           (* e:  31 c8                   xor    eax,ecx *)
           fn () => eq [0wx31, 0wxc8] (asm [xorl ecx eax])),
          ("xorl (%ecx), %eax",
           (* 10: 33 01                   xor    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx33, 0wx01] (asm [xorl (&ecx) eax])),
          (* 12: 33 41 01                xor    eax,DWORD PTR [ecx+0x1] *)
          (* 15: 33 81 00 01 00 00       xor    eax,DWORD PTR [ecx+0x100] *)
          (* 1b: 33 44 4a 01             xor    eax,DWORD PTR [edx+ecx*2+0x1] *)
          (* 1f: 33 44 4a 64             xor    eax,DWORD PTR [edx+ecx*2+0x64] *)

          ("xorl %ecx, (%eax)",
           (* 23: 31 08                   xor    DWORD PTR [eax],ecx *)
           fn () => eq [0wx31, 0wx08] (asm [xorl ecx (&eax)])),
          (* 25: 31 48 01                xor    DWORD PTR [eax+0x1],ecx *)
          (* 28: 31 88 00 01 00 00       xor    DWORD PTR [eax+0x100],ecx *)
          (* 2e: 31 4c 42 01             xor    DWORD PTR [edx+eax*2+0x1],ecx *)
          (* 32: 31 4c 42 64             xor    DWORD PTR [edx+eax*2+0x64],ecx  *)


          ("not %eax",
          (* 0:  f7 d0                   not    eax *)
           fn () => eq [0wxf7, 0wxd0] (asm [notl eax])),
          ("not (%eax)",
           (* 2:  f7 10                   not    DWORD PTR [eax] *)
           fn () => eq [0wxf7, 0wx10] (asm [notl (&eax)])),
          (* 4:  f7 50 01                not    DWORD PTR [eax+0x1] *)
          (* 7:  f7 90 00 01 00 00       not    DWORD PTR [eax+0x100] *)
          (* d:  f7 54 42 01             not    DWORD PTR [edx+eax*2+0x1] *)
          (* 11: f7 54 42 64             not    DWORD PTR [edx+eax*2+0x64]  *)

          ("mull %eax",
           (* 0:  f7 e0                   mul    eax *)
           fn () => eq [0wxf7, 0wxe0] (asm [mull eax])),
          ("mull (%eax)",
           (* 2:  f7 20                   mul    DWORD PTR [eax] *)
           fn () => eq [0wxf7, 0wx20] (asm [mull (&eax)])),
          (* 4:  f7 60 01                mul    DWORD PTR [eax+0x1] *)
          (* 7:  f7 a0 00 01 00 00       mul    DWORD PTR [eax+0x100] *)
          (* d:  f7 64 42 01             mul    DWORD PTR [edx+eax*2+0x1] *)
          (* 11: f7 64 42 64             mul    DWORD PTR [edx+eax*2+0x64]  *)

          ("guard", fn () => eq [] (asm []))
      ]
end
