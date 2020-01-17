;***********************************
;开 发 者: 迷路仟 QQ:714095563 
;开发时间: 2020.01.17
;编 译 成: 无
;***********************************
;【作用】:将[属性.ini]整理成[属性库.bin],[辅助库.bin]


Structure __AttriBase
   Index.l
   LowCRC16.l  
   Address.l
   Text$
EndStructure

Structure __AttriInfo
   Count.l  
   List ListBase.__AttriBase()
EndStructure

Structure __xxxxxxInfo
   Count.l  
   Text$
   *pListBase.__AttriBase
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
#FileName$ = "属性.ini"
; 实体,属性,值
Global Dim _DimAttri.__AttriInfo($FFFF)
Global NewMap _MapAttri.__xxxxxxInfo()
Global NewList _ListAttri.__xxxxxxInfo()


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
      LineText$ = Mid(LineText$, 2)
      AttiText$ = StringField(LineText$, 1, "】")
      AttiCount = Val(StringField(LineText$, 2, "】"))
      AttiText$ = ReplaceString(AttiText$, "★", "")
      AttiText$ = ReplaceString(AttiText$, "、", "")
      AttiText$ = ReplaceString(AttiText$, "：", "")
      AttiText$ = ReplaceString(AttiText$, ":", "")
      AttiText$ = ReplaceString(AttiText$, "，","")
      AttiText$ = ReplaceString(AttiText$, ",","")
      AttiText$ = ReplaceString(AttiText$, "&lt;","<")
      AttiText$ = ReplaceString(AttiText$, "&gt;",">")
      AttiText$ = ReplaceString(AttiText$, "&amp;","/")
      AttiText$ = ReplaceString(AttiText$, "＆","/")
      AttiText$ = ReplaceString(AttiText$, "&","/")
      AttiText$ = ReplaceString(AttiText$, "●","/")
      AttiText$ = ReplaceString(AttiText$, ").","")
      AttiText$ = ReplaceString(AttiText$, "    ","")
      Select Right(AttiText$, 1) 
         Case "*"                                                : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
         Case "①","②","③","④","⑤","⑥","⑦","⑧","⑨","⑩"  : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
      EndSelect
      Select Left(AttiText$, 1) 
         Case "*", "."                                           : AttiText$ = Mid(AttiText$, 2)
      EndSelect   
      Select AttiText$
         Case "五笔(86与98)","五笔（86与98）", "五笔i","五笔86＆9","“五笔"       : AttiText$ = "五笔(86/98版)"
         Case "五笔86/9","五笔8","五笔9", "五笔86/98","五笔86/98版", "8698五笔"   : AttiText$ = "五笔(86/98版)"
         Case "五笔型86/9","8五笔","五笔86/9" ,"五笔8/9", "五笔86" , "五笔86&8"   : AttiText$ = "五笔(86/98版)"
         Case "五笔86、9", "五笔869", "五笔86&", "五笔(86/98)","五笔86/8"         : AttiText$ = "五笔(86/98版)"
         Case "五笔86&9"                                                          : AttiText$ = "五笔(86/98版)"
         Case "五笔（86版）","五笔型8", "五笔86版","86五笔","五笔86编码","五笔6"  : AttiText$ = "五笔(86版)"
         Case "奶五笔(86)", "五笔(86版)", "86五笔编码", "86版五笔", "五笔型86"    : AttiText$ = "五笔(86版)"
         Case "86版五笔编码"                                                      : AttiText$ = "五笔(86版)"
         Case "五笔（98版）","五笔98","五笔型9","98五笔","五笔98编码"             : AttiText$ = "五笔(98版)"
         Case "五笔(98版)", "五笔98版", "98版五笔编码"                            : AttiText$ = "五笔(98版)"
      EndSelect
      If AttiText$ <> #Null$
         _MapAttri(AttiText$)
         _MapAttri()\Count + AttiCount
      EndIf 
      Index+1
      If Index % 10000 = 0 
         Debug Index
      EndIf
   Wend
   
   CloseFile(FileID)
   Index = 0
   ForEach _MapAttri()

      AttriText$ = MapKey(_MapAttri())
      AddElement(_ListAttri())
      _ListAttri()\Count = _MapAttri()\Count
      _ListAttri()\Text$ = AttriText$
      Length = StringByteLength(AttriText$, #PB_Unicode)
      CRC32 = Cipher_CRC32(@AttriText$, Length) 
      AttriIdx = CRC32 >> 16 & $FFFF
      _DimAttri(AttriIdx)\Count + 1
      
      _ListAttri()\pListBase = AddElement(_DimAttri(AttriIdx)\ListBase())
      _DimAttri(AttriIdx)\ListBase()\LowCRC16 = CRC32 & $FFFF
      Index+1
      If Index % 10000 = 0 
         Debug Index
      EndIf
   Next 
   FreeMap(_MapAttri())
   SortStructuredList(_ListAttri(), 1, 0, #PB_Long)
   
   
   
   Notice$   = "中文知识图谱快速查询系统[属性库]-开发:迷路仟  格式:属性ID+数量+字符串地址 字符串采用UTF8编码"
   NoteAddr  = SizeOf(__HeaderInfo)
   NoteSize  = StringByteLength(Notice$, #PB_Ascii)+1
   NoteSize  = (NoteSize+3)/4*4 ;4字节对齐
   AttiAddr  = NoteAddr+NoteSize
   AttiCount = ListSize(_ListAttri())
   *MemData  = AllocateMemory($1000000)
   *MemAtti  = AllocateMemory($1000000)
   *pHeader.__HeaderInfo = *MemData
   With *pHeader
      \Flags      = $41474B43
      \HeadSize   = #HeadSize
      \NoteAddr   = NoteAddr
      \Version    = 1.00
      \IdxCount   = AttiCount
      \CreateTime = Date()
      \ModifyTime = Date()
      \AddCount   = 0
   EndWith
   
   PokeS(*MemData+NoteAddr, Notice$,-1, #PB_Ascii) 

   tPos = AttiAddr
   iPos = 0
   AttriIndex = 0
   ForEach _ListAttri()
      _ListAttri()\pListBase\Index   = AttriIndex
      _ListAttri()\pListBase\Address = tPos
      PokeL(*MemAtti+iPos, tPos)    : iPos+4     ;文本地址
      AttriIndex + 1
      Lenght = StringByteLength(_ListAttri()\Text$, #PB_UTF8)+1
      PokeS(*MemData+tPos, _ListAttri()\Text$, -1, #PB_UTF8) : tPos+Lenght
   Next 
   
   UseMD5Fingerprint()   
   *pHeader\IdxAddr = tPos   ;刷新索引地址
   MD5$ = Fingerprint(*MemData+NoteAddr, tPos-NoteAddr, #PB_Cipher_MD5)
   PokeS(@*pHeader\MD5, MD5$, -1,  #PB_Ascii)
   CopyMemory(*MemAtti, *MemData+tPos, iPos): tPos+iPos

   mcsFileSave_("属性库.bin", *MemData, 0, tPos, tPos)
   FreeMemory(*MemData)
   FreeMemory(*MemAtti)
   
   
   
   
   
   Notice$   = "中文知识图谱快速查询系统[辅助库]-开发:迷路仟  格式:属性ID+数量+字符串地址 字符串采用UTF8编码"
   NoteAddr  = SizeOf(__HeaderInfo)
   NoteSize  = StringByteLength(Notice$, #PB_Ascii)+1
   NoteSize  = (NoteSize+3)/4*4 ;4字节对齐
   AttiAddr  = NoteAddr+NoteSize
   AttiCount = $10000
   *MemData  = AllocateMemory($1000000)
   *MemAtti  = AllocateMemory($1000000)
   *pHeader.__HeaderInfo = *MemData
   With *pHeader
      \Flags      = $41474B43
      \HeadSize   = #HeadSize
      \NoteAddr   = NoteAddr
      \Version    = 1.00
      \IdxCount   = AttiCount
      \CreateTime = Date()
      \ModifyTime = Date()
      \AddCount   = 0
   EndWith
   
   PokeS(*MemData+NoteAddr, Notice$,-1, #PB_Ascii) 
   
   tPos = AttiAddr
   iPos = 0
   AttriIndex = 0
   ; 标志数量[2字节]+{标志[低16位，2字节]+实体名称[4字节]+属性数据段地址[4字节]}
   For k = 0 To $FFFF
      PokeL(*MemAtti+iPos, tPos)                : iPos+4
      PokeW(*MemData+tPos, _DimAttri(k)\Count)  : tPos+2
      ForEach _DimAttri(k)\ListBase()
         PokeW(*MemData+tPos, _DimAttri(k)\ListBase()\LowCRC16) : tPos+2
         PokeL(*MemData+tPos, _DimAttri(k)\ListBase()\Address)  : tPos+4 ;属性地址
         PokeL(*MemData+tPos, _DimAttri(k)\ListBase()\Index)    : tPos+4 ;属性下标
      Next 
   Next 
   
   UseMD5Fingerprint()   
   *pHeader\IdxAddr = tPos   ;刷新索引地址
   MD5$ = Fingerprint(*MemData+NoteAddr, tPos-NoteAddr, #PB_Cipher_MD5)
   PokeS(@*pHeader\MD5, MD5$, -1,  #PB_Ascii)
   CopyMemory(*MemAtti, *MemData+tPos, iPos): tPos+iPos
   
   mcsFileSave_("辅助库.bin", *MemData,  0, tPos, tPos)
   
EndIf 





























; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 43
; Folding = g
; EnableXP
; Executable = dddddd.exe