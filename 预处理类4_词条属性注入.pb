;***********************************
;开 发 者: 迷路仟 QQ:714095563 
;开发时间: 2020.01.17
;编 译 成: 词条属性注入.exe  编译成EXE，速度快
;***********************************
;【作用】:通过[辅助库.bin]将属性类别和属性值注入到[实体库.bin]
;有带界面,运行过程需要20+分钟

#EntryFlags = $45474B43  ;[CKGE]Chinese Knowledge Graph Entrys
#SubstFlags = $53474B43  ;[CKGS]Chinese Knowledge Graph Substances
#AttriFlags = $41474B43  ;[CKGA]Chinese Knowledge Graph Attributes
#AuxilFlags = $41474B43  ;[CKGA]Chinese Knowledge Graph Auxiliary
#IndexFlags = $49474B43  ;[CKGI]Chinese Knowledge Graph Index
#TextsFlags = $54474B43  ;[CKGT]Chinese Knowledge Graph Texts

#SubstName$ = "实体库.bin"    ;存放匹配功能的数据
#EntryName$ = "词条库.bin"    ;存放词条名称的数据
#AttriName$ = "属性库.bin"    ;存放基础属性的数据
#AuxilName$ = "辅助库.bin"    ;存放基础属性的数据
#IndexName$ = "索引库.bin"    ;存放属性索引的数据
#TextsName$ = "文本库.bin"    ;存放属性值文本的数据
#Progress$  = "整理进度.ini"
#TargetFile$ = "中文智能图谱数据ownthink_v2.csv"

#HeadSize  = $24
;CRC32→[实体库]→EntryAddr→[词条库]→词条文本
;               →IndexAddr→[索引库]→所有属性→AttriAddr→[属性库]→属性文本
;                                              →TextsAddr→[文本库]→属性值文本

;头部结构
Structure __HeaderInfo
   Flags.l        ; 标志
   HeadSize.w     ; 头部大小
   NoteAddr.w     ; 备注地址
   Version.f      ; 版本号
   IdxCount.l     ; 索引数量
   IdxAddr.q      ; 索引地址
   CreateTime.l   ; 创建日期
   ModifyTime.l   ; 修改日期
   AddCount.l     ; 新增次数
   MD5.b[32]      ; ＭＤ5
   Keep.l
EndStructure

Structure __SubstBase
   Flags.w
   EntryAddr.l  ;名称地址
   IndexAddr.l  ;索引地址
   Address.l
   IsFinish.l
   Entry$
EndStructure

Structure __SubstInfo
   Address.l
   Count.w
   DimSubst.__SubstBase[320]
EndStructure

Structure __AttriBase
   Flags.w
   Address.q   ;地址
   Index.l     ;索引
   Attri$
   Value$
EndStructure

Structure __AttriInfo
   Address.q
   Count.w
   DimAttri.__AttriBase[320]
EndStructure

Structure __EntryBase
   ValueText$
   *pListBase.__AttriBase
EndStructure

Structure __EntryInfo
   EntryText$
   *pSubst.__SubstBase
   List ListAttri.__EntryBase()
EndStructure

Structure __MainInfo
   SubstID.i
   EntryID.i
   AttriID.i
   AuxilID.i
   IndexID.i
   TextsID.i
   TargetID.i
   ;===========
   IndexAddr.q
   TextsAddr.q
   IndexCount.l
   TextsCount.l   
   
   CurrEntry$
   Entry.__EntryInfo
   PrevAttri$
   *pDefaultAttri.__AttriBase
   *MemAttri
   *MemTexts
   ;===========
   ThreadID.i
EndStructure


#winScreen = 0
#btnScreen = 1
#lblScreen1 = 2
#lblScreen2 = 3
#AttiText$ = "实体类"


Global Dim _DimSubst.__SubstInfo($FFFF)
Global Dim _DimAttri.__AttriInfo($FFFF)



Global _Main.__MainInfo
; Global NewList _ListText$()


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


Procedure Cipher_EntryText(EntryText$) 
   Length = StringByteLength(EntryText$, #PB_Unicode)
   CRC32 = Cipher_CRC32(@EntryText$, Length)  ;进行CRC32计算
   Debug EntryText$
   Index.l = CRC32 >> 16 & $FFFF             ;获取高16为索引
   Flags.l = CRC32 & $FFFF                   ;获取低16为标志
;    Debug "CRC32 = 0x" + Hex(CRC32, #PB_Long)
;    Debug "Index = 0x" + Hex(Index, #PB_Long)
;    Debug "Flags = 0x" + Hex(Flags, #PB_Long)
;    Debug "Count = " + _DimSubst(Index)\Count
   
   For k = 0 To _DimSubst(Index)\Count-1
;       Debug _DimSubst(Index)\DimSubst[k]\Value$
      If Flags = _DimSubst(Index)\DimSubst[k]\Flags & $FFFF
         If EntryText$ = _DimSubst(Index)\DimSubst[k]\Entry$
            If _DimSubst(Index)\DimSubst[k]\IsFinish = #True
;                AddElement(_ListText$())
;                _ListText$() = EntryText$
            Else 
;             Debug EntryText$
               ProcedureReturn @_DimSubst(Index)\DimSubst[k]
            EndIf 
         EndIf 
      EndIf 
   Next 
   ProcedureReturn #False
EndProcedure

Procedure Cipher_AttriText(AttiText$) 
   Length = StringByteLength(AttiText$, #PB_Unicode)
   CRC32 = Cipher_CRC32(@AttiText$, Length)  ;进行CRC32计算

   Index.l = CRC32 >> 16 & $FFFF             ;获取高16为索引
   Flags.l = CRC32 & $FFFF                   ;获取低16为标志
;    Debug "CRC32 = 0x" + Hex(CRC32, #PB_Long)
;    Debug "Index = 0x" + Hex(Index, #PB_Long)
;    Debug "Flags = 0x" + Hex(Flags, #PB_Long)
;    Debug "Count = " + _DimAttri(Index)\Count
   
   For k = 0 To _DimAttri(Index)\Count-1
;       Debug _DimAttri(Index)\DimAttri[k]\Value$
      If Flags = _DimAttri(Index)\DimAttri[k]\Flags & $FFFF
         If AttiText$ = _DimAttri(Index)\DimAttri[k]\Attri$
            If _Main\Entry\pSubst
;                Debug "【"+_Main\Entry\pSubst\Entry$ + "】" + AttiText$ 
            Else 
               Debug "【初始化】" + AttiText$ 
            EndIf 
            ProcedureReturn @_DimAttri(Index)\DimAttri[k]
         EndIf 
      EndIf 
   Next 
   ProcedureReturn #False
EndProcedure

;-
Procedure Thread_Arrange()
   If _Main\Entry\pSubst = #Null : ProcedureReturn : EndIf 
   
   If IsFile(_Main\SubstID) = #Null 
      _Main\ThreadID = #False
      Debug "[词条库]FileID为Null: " 
      ProcedureReturn 
   EndIf 
   _Main\Entry\pSubst\IsFinish  = #True
   _Main\Entry\pSubst\IndexAddr = _Main\IndexAddr
   
   ;完成实体库
   FileSeek (_Main\SubstID, _Main\Entry\pSubst\Address)
   WriteLong(_Main\SubstID, _Main\IndexAddr)  
   Debug "0x" + Hex(_Main\Entry\pSubst\Address) + ": 0x" + Hex(_Main\IndexAddr)
   FlushFileBuffers(_Main\SubstID)
   
   ;整理属性和属性值
   ;属性索引段: Count+{属性IDX<<40|属性值Addr}
   iPos.q = 0  
   tPos.q = 0; 
   CountAttri = ListSize(_Main\Entry\ListAttri())
   PokeW(_Main\MemAttri+iPos, CountAttri)          : iPos+2
   ForEach _Main\Entry\ListAttri()
      TextsAddr.q = (tPos+_Main\TextsAddr)
      AttriIndex.q  = _Main\Entry\ListAttri()\pListBase\Index
      Address.q =  AttriIndex << 40
      Address.q | TextsAddr
      PokeQ(_Main\MemAttri+iPos, Address)  : iPos+8
      
      Text$ = _Main\Entry\ListAttri()\ValueText$
;       Debug Text$
      Lenght = StringByteLength(Text$, #PB_UTF8)+1
      PokeS(_Main\MemTexts+tPos, Text$, -1, #PB_UTF8) : tPos+Lenght      
   Next 
   
   ;注入属性值[文本内容]
   FileSeek (_Main\TextsID, _Main\TextsAddr)
   WriteData(_Main\TextsID, _Main\MemTexts, tPos)
   FlushFileBuffers(_Main\TextsID)
   
   NewTextsAddr.q = _Main\TextsAddr+tPos
   FileSeek (_Main\TextsID, OffsetOf(__HeaderInfo\IdxAddr))
   WriteQuad(_Main\TextsID, NewTextsAddr)
   FlushFileBuffers(_Main\TextsID)
   
   _Main\TextsCount+CountAttri
   FileSeek (_Main\TextsID, OffsetOf(__HeaderInfo\IdxCount))
   WriteLong(_Main\TextsID, _Main\TextsCount)   
   FlushFileBuffers(_Main\TextsID)
   _Main\TextsAddr = NewTextsAddr
   
   
   ;注入属性值[索引组]
   FileSeek (_Main\IndexID, _Main\IndexAddr)
   WriteData(_Main\IndexID, _Main\MemAttri, iPos)
   FlushFileBuffers(_Main\IndexID)
   
   NewIndexAddr.q = _Main\IndexAddr+iPos   
   FileSeek (_Main\IndexID, OffsetOf(__HeaderInfo\IdxAddr))
   WriteQuad(_Main\IndexID, NewIndexAddr)  
   FlushFileBuffers(_Main\TextsID)
   
   _Main\IndexCount+CountAttri
   FileSeek (_Main\IndexID, OffsetOf(__HeaderInfo\IdxCount))
   WriteLong(_Main\IndexID, _Main\IndexCount)     
   
   FlushFileBuffers(_Main\IndexID)
   _Main\IndexAddr = NewIndexAddr
   
   
   FillMemory(_Main\MemTexts, tPos, 0)
   FillMemory(_Main\MemAttri, iPos, 0)
   
   ClearList(_Main\Entry\ListAttri())
EndProcedure


Procedure Thread_Init() 
   ; #AttriName$ = ".bin"    ;存放基础属性的数据
   ; #IndexName$ = ".bin"    ;存放属性索引的数据
   ; #TextsName$ = ".bin"    ;存放属性值文本的数据
   SetGadgetText(#lblScreen1, "正在初始化....")
   
   ;====== "目标文件.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 目标文件.bin")
   If IsFile(_Main\TargetID) : CloseFile(_Main\TargetID) : EndIf     
   FileID = ReadFile(#PB_Any, #TargetFile$)
   If FileID
      _Main\TargetID = FileID
      Format = ReadStringFormat(FileID)
   Else 
      MessageRequester("", "["+#TargetFile$+"]文件不存在")
      ProcedureReturn #False
   EndIf 

   ;====== "索引库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 索引库.bin")
   If IsFile(_Main\IndexID) : CloseFile(_Main\IndexID) : EndIf   
   FileSize = FileSize(#IndexName$)
   If FileSize = -1
      Notice$   = "中文知识图谱快速查询系统〖索引库〗 开发:迷路仟  用来存放词条的属性索引信息."
      NoteAddr  = SizeOf(__HeaderInfo)
      NoteSize  = StringByteLength(Notice$, #PB_Ascii)+1
      NoteSize  = (NoteSize+3)/4*4        ;4字节对齐
      IdxAddr   = NoteAddr+NoteSize

      *MemIndex  = AllocateMemory($10000)
      *pHeader.__HeaderInfo = *MemIndex
      With *pHeader
         \Flags      = #IndexFlags
         \HeadSize   = #HeadSize
         \NoteAddr   = NoteAddr
         \Version    = 1.00
         \IdxCount   = 0
         \IdxAddr    = IdxAddr
         \CreateTime = Date()
         \ModifyTime = Date()
         \AddCount   = 0
         PokeS(*MemIndex+NoteAddr, Notice$,-1, #PB_Ascii) 
         DataSize    = IdxAddr
      EndWith
      FileID = CreateFile(#PB_Any, #IndexName$)
      If FileID
         _Main\IndexID    = FileID
         _Main\IndexAddr  = IdxAddr
         _Main\IndexCount = 0
         WriteData(FileID, *MemIndex, DataSize)
         FlushFileBuffers(FileID)
         FreeMemory(*MemIndex)
      Else 
         FreeMemory(*MemIndex)
         MessageRequester("", "["+#IndexName$+"]文件创建失败.")
         ProcedureReturn #False
      EndIf 
   Else 
      FileID = OpenFile(#PB_Any, #IndexName$)
      If FileID
         ReadSize  = SizeOf(__HeaderInfo)
         *pHeader.__HeaderInfo = AllocateMemory(ReadSize)
         ReadData(FileID, *pHeader, ReadSize)
         _Main\IndexID   = FileID
         _Main\IndexAddr  = *pHeader\IdxAddr
         _Main\IndexCount = *pHeader\IdxCount
         FreeMemory(*pHeader)
      Else 
         MessageRequester("", "["+#IndexName$+"]文件不存在")
         ProcedureReturn #False
      EndIf      
   EndIf      
   
   ;====== "文本库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 文本库.bin")
   If IsFile(_Main\TextsID) : CloseFile(_Main\TextsID) : EndIf   
   FileSize = FileSize(#TextsName$)
   If FileSize = -1
      Notice$   = "中文知识图谱快速查询系统〖文本库〗 开发:迷路仟  用来存放词条的属性值信息."
      NoteAddr  = SizeOf(__HeaderInfo)
      NoteSize  = StringByteLength(Notice$, #PB_Ascii)+1
      NoteSize  = (NoteSize+3)/4*4        ;4字节对齐
      IdxAddr   = NoteAddr+NoteSize

      *MemTexts  = AllocateMemory($10000)
      *pHeader.__HeaderInfo = *MemTexts
      With *pHeader
         \Flags      = #TextsFlags
         \HeadSize   = #HeadSize
         \NoteAddr   = NoteAddr
         \Version    = 1.00
         \IdxCount   = 0
         \IdxAddr    = IdxAddr
         \CreateTime = Date()
         \ModifyTime = Date()
         \AddCount   = 0
         PokeS(*MemIndex+NoteAddr, Notice$,-1, #PB_Ascii) 
         DataSize    = IdxAddr
      EndWith
      FileID = CreateFile(#PB_Any, #TextsName$)
      If FileID
         _Main\TextsID    = FileID
         _Main\TextsAddr  = IdxAddr
         _Main\TextsCount = 0
         WriteData(FileID, *MemTexts, DataSize)
         FlushFileBuffers(FileID)
         FreeMemory(*MemTexts)
      Else 
         FreeMemory(*MemTexts)
         MessageRequester("", "["+#TextsName$+"]文件创建失败.")
         ProcedureReturn #False
      EndIf 
   Else 
      FileID = OpenFile(#PB_Any, #TextsName$)
      If FileID
         ReadSize  = SizeOf(__HeaderInfo)
         *pHeader.__HeaderInfo = AllocateMemory(ReadSize)
         ReadData(FileID, *pHeader, ReadSize)
         _Main\TextsID    = FileID
         _Main\TextsAddr  = *pHeader\IdxAddr
         _Main\TextsCount = *pHeader\IdxCount
         FreeMemory(*pHeader)
      Else 
         MessageRequester("", "["+#TextsName$+"]文件不存在")
         ProcedureReturn #False
      EndIf      
   EndIf    
   
   
   ;====== "属性库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 属性库.bin")
   If IsFile(_Main\AttriID) : CloseFile(_Main\AttriID) : EndIf     
   FileID = ReadFile(#PB_Any, #AttriName$)
   If FileID
      _Main\AttriID = FileID
      FileSize      = FileSize(#AttriName$)
      *MemAttri     = AllocateMemory(FileSize)
      ReadData(FileID, *MemAttri, FileSize)
      CloseFile(FileID)
   Else 
      MessageRequester("", "["+#AttriName$+"]文件不存在")
      ProcedureReturn #False
   EndIf 
   SetGadgetText(#lblScreen1, "正在初始化完毕.")
   
   ;====== "辅助库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 辅助库.bin")
   If IsFile(_Main\AuxilID) : CloseFile(_Main\AuxilID) : EndIf     
   FileID = OpenFile(#PB_Any, #AuxilName$)
   If FileID
      FileSize = FileSize(#AuxilName$)
      *MemAuxil = AllocateMemory(FileSize)
      ReadData(FileID, *MemAuxil, FileSize)
      *pHeader.__HeaderInfo = *MemAuxil
      For k = 0 To $FFFF
         Pos   = PeekL(*MemAuxil+*pHeader\IdxAddr+k*4)
         Count = PeekW(*MemAuxil+Pos)   : Pos+2 ;获取标志数量
         _DimAttri(k)\Count = Count
         With _DimAttri(k)\DimAttri[i] 
            For i = 0 To Count-1
               If _Main\ThreadID = #False : Break 2 : EndIf  
               \Flags     = PeekW(*MemAuxil+Pos)   : Pos+2
               \Address   = PeekL(*MemAuxil+Pos)   : Pos+4
               \Index     = PeekL(*MemAuxil+Pos)   : Pos+4
               \Attri$    = PeekS(*MemAttri+\Address, -1, #PB_UTF8)
               
;                Debug Hex(\Index)
;                If \Attri$ = #AttiText$
; ;                   Debug "属性 0x"+Hex(\Index) +"/ 0x"+Hex(k)+":"+Str(Count) +":" + \Attri$
;                EndIf 
            Next 
         EndWith
         If Random(100) = 0
            SetGadgetText(#lblScreen2, Str(k+1)+"/"+Str($10000)) 
         EndIf 
      Next 
      SetGadgetText(#lblScreen2, Str($10000)+"/"+Str($10000)) 
      _Main\AuxilID = FileID
      CloseFile(FileID)
      FreeMemory(*MemAttri)
      FreeMemory(*MemAuxil)      
      If _Main\ThreadID = #False
         SetGadgetText(#lblScreen1, "中止初始化.")
         ProcedureReturn #False
      EndIf 
   Else 
      FreeMemory(*MemAttri)
      MessageRequester("", "["+#AuxilName$+"]文件不存在")
      ProcedureReturn #False
   EndIf 

   ;====== "词条库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 词条库.bin")
   If IsFile(_Main\EntryID) : CloseFile(_Main\EntryID) : EndIf     
   FileID = ReadFile(#PB_Any, #EntryName$)
   If FileID
      _Main\EntryID = FileID
      FileSize = FileSize(#EntryName$)
      *MemEntry = AllocateMemory(FileSize)
      ReadData(FileID, *MemEntry, FileSize)
;       CloseFile(FileID)
   Else 
      MessageRequester("", "["+#EntryName$+"]文件不存在")
      ProcedureReturn #False
   EndIf 
   
   ;====== "实体库.bin" ======
   SetGadgetText(#lblScreen1, "正在初始化: 实体库.bin")
   If IsFile(_Main\SubstID) : CloseFile(_Main\SubstID) : EndIf     
   FileID = OpenFile(#PB_Any, #SubstName$)
   If FileID
      FileSize = FileSize(#SubstName$)
      *MemSubst = AllocateMemory(FileSize)
      ReadData(FileID, *MemSubst, FileSize)
      *pHeader.__HeaderInfo = *MemSubst
      For k = 0 To $FFFF
         Pos = PeekL(*MemSubst+*pHeader\IdxAddr+k*4)
         Count = PeekW(*MemSubst+Pos)   : Pos+2 ;获取标志数量
         
         _DimSubst(k)\Count = Count
         For i = 0 To Count-1
            With _DimSubst(k)\DimSubst[i] 
               If _Main\ThreadID = #False : Break 2 : EndIf  
               \Flags     = PeekW(*MemSubst+Pos)   : Pos+2
               \EntryAddr = PeekL(*MemSubst+Pos)   : Pos+4
               \Address   = Pos
               \IndexAddr = PeekL(*MemSubst+Pos)   : Pos+4
               \Entry$    = PeekS(*MemEntry+\EntryAddr, -1, #PB_UTF8)
               If \IndexAddr
                  \IsFinish = #True
               EndIf 
            EndWith
          Next 
         If Random(100) = 0
            SetGadgetText(#lblScreen2, Str(k+1)+"/"+Str($10000)) 
         EndIf 
      Next 
      SetGadgetText(#lblScreen2, Str($10000)+"/"+Str($10000)) 
      _Main\SubstID = FileID
      FreeMemory(*MemEntry)
      FreeMemory(*MemSubst)      
      If _Main\ThreadID = #False
         CloseFile(FileID)
         SetGadgetText(#lblScreen1, "中止初始化.")
         ProcedureReturn #False
      EndIf 
   Else 
      FreeMemory(*MemEntry)
      MessageRequester("", "["+#SubstName$+"]文件不存在")
      ProcedureReturn #False
   EndIf     

   _Main\pDefaultAttri = #Null

   _Main\pDefaultAttri = Cipher_AttriText(#AttiText$) 
   If _Main\pDefaultAttri = #Null
      MessageRequester("", "默认属性不存在.")
      ProcedureReturn #False
   EndIf 
;    Debug "默认属性: "
;    Debug "Index = " + Str(_Main\pDefaultAttri\Index)
;    Debug "Flags = 0x" + Hex(_Main\pDefaultAttri\Flags)
;    Debug "Value = " + _Main\pDefaultAttri\Attri$
   
   ProcedureReturn #True
EndProcedure

Procedure Thread_Parser(LineText$)
   ; ===========　分析实体
   Index = 1 : CountParser = CountString(LineText$, ",")+1
   ;获取第一段的内容，
   EntryText$ = StringField(LineText$, Index, ",")     
   ;判断内容是否为空
   EntryText$ = ReplaceString(EntryText$, " ", "")
   EntryText$ = ReplaceString(EntryText$, "　", "")
   If EntryText$ = #Null$ : ProcedureReturn : EndIf 
   ;判断内容是否包含双引号
   Count = CountString(EntryText$, #DQUOTE$)
   If Count 
      ;判断双引号是不是在第一个字节
      If Left(EntryText$, 1) = #DQUOTE$
         EntryText$ = Mid(EntryText$, 2)
         EntryText$ = ReplaceString(EntryText$, #DQUOTE$+#DQUOTE$, "@")
         NameCount = CountString(EntryText$, #DQUOTE$)+1
         While NameCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            EntryText$ = EntryText$ +","+ PartText$
            NameCount = CountString(EntryText$, #DQUOTE$)+1
         Wend 
         EntryText$ = Mid(EntryText$, 1, Len(EntryText$)-1)
         EntryText$ = ReplaceString(EntryText$, "@", #DQUOTE$)
      Else 
         EntryText$ = ReplaceString(EntryText$, #DQUOTE$+#DQUOTE$, "@")
         NameCount = CountString(EntryText$, #DQUOTE$)+0
         While NameCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            EntryText$ = EntryText$ +","+ PartText$
            NameCount = CountString(EntryText$, #DQUOTE$)+0
         Wend 
         EntryText$ = ReplaceString(EntryText$, "@", #DQUOTE$)
      EndIf 
   EndIf 
   
   NoteText$ = #Null$
   If FindString(EntryText$, "[")
      NoteText$  = StringField(EntryText$, 2, "[")
      NoteText$  = Mid(NoteText$, 1, Len(NoteText$)-1)
      EntryText$ = StringField(EntryText$, 1, "[")
      If EntryText$ = NoteText$
         NoteText$  =  #Null$
      EndIf 
   EndIf 
      
   If EntryText$ <> _Main\CurrEntry$ And EntryText$ <> #Null$
      If NoteText$ = #Null$
         Thread_Arrange()
         _Main\Entry\EntryText$ = EntryText$
         _Main\CurrEntry$  = EntryText$
      Else 
         Thread_Arrange()
         AddElement(_Main\Entry\ListAttri())
         _Main\Entry\ListAttri()\ValueText$ = NoteText$
         _Main\Entry\ListAttri()\pListBase = _Main\pDefaultAttri
         _Main\Entry\EntryText$ = EntryText$
         _Main\CurrEntry$       = EntryText$
      EndIf 
      *pSubst.__SubstBase = Cipher_EntryText(EntryText$)
      _Main\Entry\pSubst = *pSubst
      If *pSubst = #Null
;          _Main\ThreadID = #False
         Debug "找不到属性: " + EntryText$
         Debug "找不到属性: " + EntryText$
         Debug "找不到属性: " + EntryText$
         ProcedureReturn 
      EndIf 
   EndIf       

   
   
   ; ===========　分析属性
   Index+1
   AttiText$ = StringField(LineText$, Index, ",")
   
   ;判断内容是否为空
   AttiText$ = ReplaceString(AttiText$, " ", "")
   AttiText$ = ReplaceString(AttiText$, "　", "")
   If AttiText$ = #Null$ : ProcedureReturn : EndIf 
   
   ;判断内容是否包含双引号
   Count = CountString(AttiText$, #DQUOTE$)
   If Count 
      ;判断双引号是不是在第一个字节
      If Left(AttiText$, 1) = #DQUOTE$
         AttiText$ = Mid(AttiText$, 2)
         AttiText$ = ReplaceString(AttiText$, #DQUOTE$+#DQUOTE$, "@")
         AttiCount = CountString(AttiText$, #DQUOTE$)+1
         While AttiCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            AttiText$ = AttiText$ +","+ PartText$
            AttiCount = CountString(AttiText$, #DQUOTE$)+1
         Wend 
         AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
         AttiText$ = ReplaceString(AttiText$, "@", #DQUOTE$)
      Else 
         AttiText$ = ReplaceString(AttiText$, #DQUOTE$+#DQUOTE$, "@")
         AttiCount = CountString(AttiText$, #DQUOTE$)+0
         While AttiCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            AttiText$ = AttiText$ +","+ PartText$
            AttiCount = CountString(AttiText$, #DQUOTE$)+0
         Wend 
         AttiText$ = ReplaceString(AttiText$, "@", #DQUOTE$)
      EndIf 
   EndIf 
   AttiText$ = ReplaceString(AttiText$, "【","")
   AttiText$ = ReplaceString(AttiText$, "】", "")
   AttiText$ = ReplaceString(AttiText$, "[", "")
   AttiText$ = ReplaceString(AttiText$, "]", "")
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
      Case "1","2","3","4","5","6","7","8","9", "*"            : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
      Case "１","２","３","４","５","６","７","８","９"        : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
      Case "一","二","三","四","五","六","七","八","九", "十"  : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
      Case "①","②","③","④","⑤","⑥","⑦","⑧","⑨","⑩"   : AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
   EndSelect
   
   Select Left(AttiText$, 1) 
      Case "*", "."                                            : AttiText$ = Mid(AttiText$, 2)
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
   
   If AttiText$ = #Null$ : ProcedureReturn : EndIf 
   If AttiText$ = "." :  AttiText$ = _Main\PrevAttri$ : EndIf 
   _Main\PrevAttri$ = AttiText$
   If AttiText$ = #Null$ : ProcedureReturn : EndIf 
   
   *pAttri.__AttriBase = Cipher_AttriText(AttiText$) 
   If *pAttri = #Null
;       _Main\ThreadID = #False
      Debug "找不到属性: " + AttiText$
      Debug "找不到属性: " + AttiText$
      Debug "找不到属性: " + AttiText$
      ProcedureReturn 
   EndIf 

   ; ===========　分析值
   Index+1
   NoteText$ = StringField(LineText$, Index, ",")

   ;判断内容是否为空
   NoteText$ = ReplaceString(NoteText$, " ", "")
   NoteText$ = ReplaceString(NoteText$, "　", "")
   If NoteText$ = #Null$ : ProcedureReturn : EndIf 
   
   ;判断内容是否包含双引号
   Count = CountString(NoteText$, #DQUOTE$)
   If Count 
      ;判断双引号是不是在第一个字节
      If Left(NoteText$, 1) = #DQUOTE$
         NoteText$ = Mid(NoteText$, 2)
         NoteText$ = ReplaceString(NoteText$, #DQUOTE$+#DQUOTE$, "@")
         NoteCount = CountString(NoteText$, #DQUOTE$)+1
         While NoteCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            NoteText$ = NoteText$ +","+ PartText$
            NoteCount = CountString(NoteText$, #DQUOTE$)+1
         Wend 
         NoteText$ = Mid(NoteText$, 1, Len(NoteText$)-1)
         NoteText$ = ReplaceString(NoteText$, "@", #DQUOTE$)
      Else 
         NoteText$ = ReplaceString(NoteText$, #DQUOTE$+#DQUOTE$, "@")
         NoteCount = CountString(NoteText$, #DQUOTE$)+0
         While NoteCount % 2 = 1 And Index <= CountParser 
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            NoteText$ = NoteText$ +","+ PartText$
            NoteCount = CountString(NoteText$, #DQUOTE$)+0
         Wend 
         NoteText$ = ReplaceString(NoteText$, "@", #DQUOTE$)
      EndIf 
   EndIf 

   AddElement(_Main\Entry\ListAttri())
   _Main\Entry\ListAttri()\ValueText$ = NoteText$
   _Main\Entry\ListAttri()\pListBase  = *pAttri

EndProcedure

Procedure Thread(Index)
   
   Result = Thread_Init() 
   If Result = #False : ProcedureReturn : EndIf 

   OpenPreferences(#Progress$)
      PreferenceGroup("整理进度")
      PrevAddress.q = ReadPreferenceQuad("数据地址", 0) 
      PrevIndex.q   = ReadPreferenceQuad("数据索引", 0) 
   ClosePreferences()   
         
   NewList ListText$()
   _Main\Entry\pSubst = #Null
   _Main\MemAttri = AllocateMemory($1000000)
   _Main\MemTexts = AllocateMemory($1000000)
   StartTimer = mcsGetTime_()
   FileSeek(_Main\TargetID, PrevAddress)    
   Format = #PB_UTF8      
   While Eof(_Main\TargetID) = 0 
      CurrAddress.q = Loc(_Main\TargetID)
      LineText$ = ReadString(_Main\TargetID, Format)
      ;===========　加载这完整的文本
      Count = CountString(LineText$, #DQUOTE$)
      If Count 
         While Count % 2 = 1
            LineText$ = LineText$ +"\n"+ ReadString(_Main\TargetID, Format)
            Count = CountString(LineText$, #DQUOTE$)
         Wend            
      EndIf 
   Debug LineText$
      Thread_Parser(LineText$)
      
      If _Main\ThreadID = #True
         PrevIndex+1
         If PrevIndex % 1000 = 0
            SetGadgetText(#lblScreen1, Str(PrevIndex)+"/139,951,301 - " + Str(PrevIndex/1399513)+"% -" +_Main\TextsCount)
;             Debug Str(PrevIndex)+"/15,207,500"
;             While WindowEvent() : Wend
         EndIf 
         PrevAddress = CurrAddress
         CurrTimer = mcsGetTime_(StartTimer)
         If CurrTimer > NextTimer 
            SetGadgetText(#lblScreen2, FormatDate("%hh:%ii:%ss", NextTimer/1000)) 
            NextTimer = NextTimer+1000
         EndIf  
      Else 
         Break
      EndIf 
;       If PrevIndex % 140000 = 0
;          AddElement(ListText$()) : ListText$() = _Main\CurrEntry$
;       EndIf 
    Wend 
;     If _Main\ThreadID = #True 
;        Thread_Arrange()
;     EndIf 
;    If CreateFile(0, "重复词条.txt")
;       WriteStringFormat(0, #PB_UTF8)
;       ForEach _ListText$()
;          WriteStringN(0,  _ListText$(), #PB_UTF8)
;       Next 
;       CloseFile(0)
;    EndIf 
   
   If CreatePreferences(#Progress$)
      PreferenceGroup("整理进度")
         WritePreferenceQuad("数据地址", PrevAddress)
         WritePreferenceQuad("数据索引", PrevIndex)
      ClosePreferences()
   EndIf
   SetGadgetText(#lblScreen1, Str(PrevIndex)+"/139,951,301 - " + Str(PrevIndex/1399513)+"% -" +_Main\TextsCount)
   
   If IsFile(_Main\SubstID) : CloseFile(_Main\SubstID) : EndIf  
   If IsFile(_Main\EntryID) : CloseFile(_Main\EntryID) : EndIf  
   If IsFile(_Main\AttriID) : CloseFile(_Main\AttriID) : EndIf  
   If IsFile(_Main\IndexID) : CloseFile(_Main\IndexID) : EndIf  
   If IsFile(_Main\TextsID) : CloseFile(_Main\TextsID) : EndIf 
   If IsFile(_Main\AuxilID) : CloseFile(_Main\AuxilID) : EndIf 
   
   FreeMemory(_Main\MemAttri)
   FreeMemory(_Main\MemTexts)
   
   _Main\ThreadID = #False
   SetGadgetText(#btnScreen, "开始") 
   MessageRequester("", "完成")
EndProcedure
 

WindowFlags = #PB_Window_ScreenCentered| #PB_Window_SystemMenu| #PB_Window_MinimizeGadget|
              #PB_Window_MaximizeGadget| #PB_Window_SizeGadget 
hWindow = OpenWindow(#winScreen, 0, 0, 400, 250, "整理程序", WindowFlags)

TextGadget  (#lblScreen1, 000, 100, 400, 020, #SubstName$, #PB_Text_Center)
TextGadget  (#lblScreen2, 000, 070, 400, 020, #SubstName$, #PB_Text_Center)
ButtonGadget(#btnScreen, 150, 150, 100, 050, "开始", #PB_Button_Toggle)

Repeat
   WinEvent  = WindowEvent()
   Select WinEvent
      Case #PB_Event_CloseWindow       
         IsExitWindow = #True
      Case #PB_Event_Gadget
         If EventGadget() = #btnScreen
            If GetGadgetState(#btnScreen) = #False
               _Main\ThreadID = #False
               SetGadgetText(#btnScreen, "开始") 
            Else 
               _Main\ThreadID = #True
               SetGadgetText(#btnScreen, "停止")
;    Thread(0)
               CreateThread(@Thread(), 0)
            EndIf 
         EndIf 
   EndSelect      
   Delay(1)
Until IsExitWindow = #True

End










; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 7
; Folding = AA5
; EnableXP
; Executable = 词条属性注入.exe