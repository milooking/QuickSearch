;***********************************
;开 发 者: 迷路仟 QQ:714095563 
;开发时间: 2020.01.17
;编 译 成: 无
;***********************************
;【作用】:将[实体.ini]整理成[实体库.bin],[词条库.bin]

Structure __AttiInfo
   LowCRC16.l  
   Text$
EndStructure


Structure __SubsInfo
   Count.l   
   List ListAtti.__AttiInfo()
EndStructure

;Chinese Knowledge Graph Substances
;Chinese Knowledge Graph Entrys
Structure __HeaderInfo
   Flags.l        ; 标志CKGS
   HeadSize.w     ; 头部大小
   NoteAddr.w     ; 备注地址
   Version.f      ; 版本号
   IdxCount.l     ; 属性数量
   IdxAddr.q      ; 索引地址
   CreateTime.l   ; 创建日期
   ModifyTime.l   ; 修改日期
   AddCount.l     ; 新增次数
   MD5.b[32]      ; ＭＤ5
   Keep.l
EndStructure

#HeadSize  = $24
#FileName$ = "实体.ini"
; 实体,属性,值
Global Dim _DimSubs.__SubsInfo($FFFF)

;知识图谱实体.bin　;存放匹配功能的数据
;知识图谱词条.bin　;存放词条名称的数据
;知识图谱索引.bin　;存放属性索引的数据
;知识图谱属性.bin　;存放基础属性的数据
;知识图谱数据.bin　;存放属性值的数据

;匹配过程：先CRC32取高16位做为索引，找到块；然后再用低16位找到目标，读取字符串进行对比，如果不是目标，再读下搜索下一个目标，再做对比。
;资源结构[倒置结构]：头部+数据块+索引区
;索引区结构: 数据段地址[4字节]，按高16位做为索引进行存放
;数据块结构: 标志数量[2字节]+{标志[低16位，2字节]+实体名称[4字节]+属性数据段地址[4字节]}，顺序排列，将1.4亿次的匹配量，降到170-300次的匹配量





;CRC32校验
Procedure.l Cipher_CRC32(*pMemData, DataSize) 
   !MOV Esi, dword [p.p_pMemData] ;esi = ptr to *pMemData
   !MOV Edi, dword [p.v_DataSize] ;edi = length of *pMemData 
   !MOV Ecx, -1                 ;ecx = -1 
   !MOV Edx, Ecx                ;edx = -1 
   !_crc321:                    ;"nextbyte" next byte from *pMemData
   !XOR Eax, Eax                ;eax = 0 
   !XOR Ebx, Ebx                ;ebx = 0 
   !DB 0xAC                     ;"lodsb" instruction to get next byte
   !XOR al, cl                  ;xor al with cl 
   !MOV cl, ch                  ;cl = ch 
   !MOV ch, dl                  ;ch = dl 
   !MOV dl, dh                  ;dl = dh
   !MOV dh, 8                   ;dh = 8 
   !_crc322:                    ;"nextbit" next bit in the byte
   !SHR bx, 1                   ;shift bits in bx right by 1 
   !RCR ax, 1                   ;(rotate through carry) bits in ax by 1 
   !JNC near _crc323            ;jump to "nocarry" if carry flag not set 
   !XOR ax, 0x08320             ;xor ax with 33568 
   !XOR bx, 0x0EDB8             ;xor bx with 60856
   !_crc323:                    ;"nocarry" if carry flag wasn't set
   !DEC dh                      ;dh = dh - 1 
   !JNZ near _crc322            ;if dh isnt zero, jump to "nextbit" 
   !XOR Ecx, Eax                ;xor ecx with eax 
   !XOR Edx, Ebx                ;xor edx with ebx 
   !DEC Edi                     ;finished with that byte, decrement counter 
   !JNZ near _crc321            ;if edi counter isnt at 0, jump to "nextbyte" 
   !NOT Edx                     ;invert edx bits - 1s complement 
   !NOT Ecx                     ;invert ecx bits - 1s complement 
   !MOV Eax, Edx                ;mov edx into eax 
   !ROL Eax, 16                 ;rotate bits in eax left by 16 places 
   !MOV ax, cx                  ;mov cx into ax 
  ProcedureReturn
EndProcedure

FileID = ReadFile(#PB_Any, #FileName$) 
If FileID
   Format = ReadStringFormat(FileID)
   Format = #PB_UTF8
   While Eof(FileID) = 0
      LineText$ = ReadString(FileID, Format)
      If LineText$ <> #Null$ 
         Length = StringByteLength(LineText$, #PB_Unicode)
         CRC32 = Cipher_CRC32(@LineText$, Length) 
         SubsIdx = CRC32 >> 16 & $FFFF
         _DimSubs(SubsIdx)\Count + 1
         AddElement(_DimSubs(SubsIdx)\ListAtti())
         _DimSubs(SubsIdx)\ListAtti()\LowCRC16 = CRC32 & $FFFF
         _DimSubs(SubsIdx)\ListAtti()\Text$    = LineText$
         Index+1
         If Index % 10000 = 0 
            Debug Index
         EndIf
      EndIf 
   Wend
   CloseFile(FileID)
EndIf 
;    ClearDebugOutput()
;    For k = 0 To $FFFF
;       Debug "["+RSet(Hex(k), 4, "0")+"]:"+ _DimSubs(k)\Count
;    Next 
; End
; 

Notice$   = "中文知识图谱快速查询系统〖实体库〗 开发:迷路仟  顺序排列,将1.4亿次的匹配量,降到170-300次的匹配量."
NoteAddr  = SizeOf(__HeaderInfo)
NoteSize  = StringByteLength(Notice$, #PB_Ascii)+1
NoteSize  = (NoteSize+3)/4*4        ;4字节对齐
SubsAddr  = NoteAddr+NoteSize
*MemData  = AllocateMemory($A000000)
*MemSubs  = AllocateMemory($1000000)
*pHeader.__HeaderInfo = *MemData
With *pHeader
   \Flags      = $53474B43
   \HeadSize   = #HeadSize
   \NoteAddr   = NoteAddr
   \Version    = 1.00
   \IdxCount   = $10000
   \CreateTime = Date()
   \ModifyTime = Date()
   \AddCount   = 0
EndWith

PokeS(*MemData+NoteAddr, Notice$,-1, #PB_Ascii) 


;==================================
Notice2$   = "中文知识图谱快速查询系统〖词条库〗 开发:迷路仟  用于记录各个词条的文本内容."
NoteAddr2  = SizeOf(__HeaderInfo)
NoteSize2  = StringByteLength(Notice2$, #PB_Ascii)+1
NoteSize2  = (NoteSize2+3)/4*4        ;4字节对齐
EntryAddr  = NoteAddr2+NoteSize2
*MemEntry  = AllocateMemory($18000000)
*pHeader2.__HeaderInfo = *MemEntry
With *pHeader2
   \Flags      = $45474B43
   \HeadSize   = #HeadSize
   \NoteAddr   = NoteAddr2
   \Version    = 1.00
   \IdxCount   = Index
   \CreateTime = Date()
   \ModifyTime = Date()
   \AddCount   = 0
EndWith
PokeS(*MemEntry+NoteAddr2, Notice2$, -1, #PB_Ascii) 


tPos = SubsAddr
iPos = 0
ePos = EntryAddr

; 标志数量[2字节]+{标志[低16位，2字节]+实体名称[4字节]+属性数据段地址[4字节]}
For k = 0 To $FFFF
   PokeL(*MemSubs+iPos, tPos) : iPos+4
   PokeW(*MemData+tPos, _DimSubs(k)\Count) : tPos+2
   ForEach _DimSubs(k)\ListAtti()

      PokeW(*MemData+tPos, _DimSubs(k)\ListAtti()\LowCRC16) : tPos+2
      PokeL(*MemData+tPos, ePos) : tPos+4 ;实体名称
      PokeL(*MemData+tPos, 0)    : tPos+4
      
      
      Lenght = StringByteLength(_DimSubs(k)\ListAtti()\Text$, #PB_UTF8)+1
      PokeS(*MemEntry+ePos, _DimSubs(k)\ListAtti()\Text$, -1, #PB_UTF8) : ePos+Lenght      
   Next 
Next 

   UseMD5Fingerprint()   
   *pHeader\IdxAddr = tPos   ;刷新索引地址
   MD5$ = Fingerprint(*MemData+NoteAddr, tPos-NoteAddr, #PB_Cipher_MD5)
   PokeS(@*pHeader\MD5, MD5$, -1,  #PB_Ascii)
   CopyMemory(*MemSubs, *MemData+tPos, iPos): tPos+iPos
   
   mcsFileSave_("实体库.bin", *MemData,  0, tPos, tPos)
   
   mcsFileSave_("词条库.bin", *MemEntry, 0, ePos, ePos)
   
   
   
   
   
   
   
   
   
   
   
   
; 
; ;知识图谱实体.bin　;存放匹配功能的数据
; ;知识图谱词条.bin　;存放词条名称的数据
; ;知识图谱索引.bin　;存放属性索引的数据





; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 12
; Folding = 4
; EnableXP
; Executable = dddddd.exe