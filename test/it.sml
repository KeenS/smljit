structure IntegrationTest = struct
    open SMLUnit
    open Assert
    open ATT
    val asm = Emit.fromInstsToBytes
    val eq = assertEqualList assertEqualWord8
    fun suite () = Test.labelTests [
          ("addl $0x1, %eax",
           (* 0:  83 c0 01                add    eax,0x1 *)
           fn () => eq [0wx83, 0wxc0, 0wx01] (asm [addl ($0x1) eax])),
          ("addl $0x400, %eax",
           (* 3:  05 00 04 00 00          add    eax,0x400 *)
           fn () => eq [0wx05, 0wx00, 0wx04, 0wx00, 0wx00] (asm [addl ($0x400) eax])),
          ("addl %ecx, %eax",
           (* 8:  01 c8                   add    eax,ecx *)
           fn () => eq [0wx01, 0wxc8] (asm [addl ecx eax])),
          ("addl (%ecx), %eax",
           (* a:  03 01                   add    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx03, 0wx01] (asm [addl (&ecx) eax])),
          ("addl %ecx, (%eax)",
           (* 0:  01 08                   add    DWORD PTR [eax],ecx *)
           fn () => eq [0wx01, 0wx08] (asm [addl ecx (&eax)])),
          (* c:  03 41 04                add    eax,DWORD PTR [ecx+0x4] *)
          (* f:  03 44 19 04             add    eax,DWORD PTR [ecx+ebx*1+0x4] *)
          (* 13: 03 44 4b 04             add    eax,DWORD PTR [ebx+ecx*2+0x4] *)
          (* 17: 03 84 4b 00 01 00 00    add    eax,DWORD PTR [ebx+ecx*2+0x100] *)

          ("adcl $0x1, %eax",
           (* 0:  83 d0 01                adc    eax,0x1 *)
           fn () => eq [0wx83, 0wxd0, 0wx01] (asm [adcl ($0x1) eax])),
          ("adcl $0x400, %eax",
           (* 3:  15 00 04 00 00          adc    eax,0x400 *)
           fn () => eq [0wx15, 0wx00, 0wx04, 0wx00, 0wx00] (asm [adcl ($0x400) eax])),
          ("adcl %ecx, %eax",
           (* 8:  11 c8                   adc    eax,ecx *)
           fn () => eq [0wx11, 0wxc8] (asm [adcl ecx eax])),
          ("adcl (%ecx) %eax",
           (* a:  13 01                   adc    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx13, 0wx01] (asm [adcl (&ecx) eax])),
          ("adcl %ecx, (%eax)",
           (* 0:  11 08                   adc    DWORD PTR [eax],ecx  *)
           fn () => eq [0wx11, 0wx08] (asm [adcl ecx (&eax)])),
          (* c:  13 41 04                adc    eax,DWORD PTR [ecx+0x4] *)
          (* f:  13 44 19 04             adc    eax,DWORD PTR [ecx+ebx*1+0x4] *)
          (* 13: 13 44 4b 04             adc    eax,DWORD PTR [ebx+ecx*2+0x4] *)
          (* 17: 13 84 4b 00 01 00 00    adc    eax,DWORD PTR [ebx+ecx*2+0x100]  *)


          ("subl $0x1, %eax",
           (* 0:  83 e8 01                sub    eax,0x1 *)
           fn () => eq [0wx83, 0wxe8, 0wx01] (asm [subl ($0x1) eax])),
          ("subl $0x400, %eax",
           (* 3:  2d 00 04 00 00          sub    eax,0x400 *)
           fn () => eq [0wx2d, 0wx00, 0wx04, 0wx00, 0wx00] (asm [subl ($0x400) eax])),
          ("subl %ecx, %eax",
           (* 8:  29 c8                   sub    eax,ecx *)
           fn () => eq [0wx29, 0wxc8] (asm [subl ecx eax])),
          ("subl (%ecx) %eax",
           (* a:  2b 01                   sub    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx2b, 0wx01] (asm [subl (&ecx) eax])),
          ("subl %ecx (%eax)",
           (* 0:  29 08                   sub    DWORD PTR [eax],ecx  *)
           fn () => eq [0wx29, 0wx08] (asm [subl ecx (&eax)])),
          (* c:  2b 41 04                sub    eax,DWORD PTR [ecx+0x4] *)
          (* f:  2b 44 19 04             sub    eax,DWORD PTR [ecx+ebx*1+0x4] *)
          (* 13: 2b 44 4b 04             sub    eax,DWORD PTR [ebx+ecx*2+0x4] *)
          (* 17: 2b 84 4b 00 01 00 00    sub    eax,DWORD PTR [ebx+ecx*2+0x100]     *)

          ("sbbl $0x1, %eax",
           (* 0:  83 d8 01                sbb    eax,0x1 *)
           fn () => eq [0wx83, 0wxd8, 0wx01] (asm [sbbl ($0x1) eax])),
          ("sbbl $0x400, %eax",
           (* 3:  1d 00 04 00 00          sbb    eax,0x400 *)
           fn () => eq [0wx1d, 0wx00, 0wx04, 0wx00, 0wx00] (asm [sbbl ($0x400) eax])),
          ("sbbl %ecx, %eax",
           (* 8:  19 c8                   sbb    eax,ecx *)
           fn () => eq [0wx19, 0wxc8] (asm [sbbl ecx eax])),
          ("sbbl (%ecx) %eax",
           (* a:  1b 01                   sbb    eax,DWORD PTR [ecx] *)
           fn () => eq [0wx1b, 0wx01] (asm [sbbl (&ecx) eax])),          
          ("sbbl %ecx (%eax)",
           (* 0:  19 08                   sbb    DWORD PTR [eax],ecx  *)
           fn () => eq [0wx19, 0wx08] (asm [sbbl ecx (&eax)])),          
          (* c:  1b 41 04                sbb    eax,DWORD PTR [ecx+0x4] *)
          (* f:  1b 44 19 04             sbb    eax,DWORD PTR [ecx+ebx*1+0x4] *)
          (* 13: 1b 44 4b 04             sbb    eax,DWORD PTR [ebx+ecx*2+0x4] *)
          (* 17: 1b 84 4b 00 01 00 00    sbb    eax,DWORD PTR [ebx+ecx*2+0x100] *)

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
          ("guard", fn () => eq [] (asm []))
      ]
end
