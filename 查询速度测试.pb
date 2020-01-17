;***********************************
;开 发 者: 迷路仟 QQ:714095563 
;开发时间: 2020.01.17
;编 译 成: [查询速度测试.exe]
;***********************************
;代码跟[查询测试Demo.pb]差不多,只是这个没有界面
;1000次查询大概65毫秒,内存峰值在30M内

;CRC32→[实体库]→EntryAddr→[词条库]→词条文本
;               →IndexAddr→[索引库]→所有属性→AttriAddr→[属性库]→属性文本
;                                              →TextsAddr→[文本库]→属性值文本

#SubstFlags = $53474B43  ;[CKGS]Chinese Knowledge Graph Substances
#EntryFlags = $45474B43  ;[CKGE]Chinese Knowledge Graph Entrys
#AttriFlags = $41474B43  ;[CKGA]Chinese Knowledge Graph Attributes
#IndexFlags = $49474B43  ;[CKGI]Chinese Knowledge Graph Index
#TextsFlags = $54474B43  ;[CKGT]Chinese Knowledge Graph Texts

#SubstName$ = "实体库.bin"    ;存放匹配功能的数据
#EntryName$ = "词条库.bin"    ;存放词条名称的数据
#AttriName$ = "属性库.bin"    ;存放基础属性的数据
#IndexName$ = "索引库.bin"    ;存放属性索引的数据
#TextsName$ = "文本库.bin"    ;存放属性值文本的数据

Enumeration
   #winScreen 
   #btnScreen
   #lblEntrys
   #txtEntrys
   #rtxResult 
EndEnumeration

;头部结构,各个库通用
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

;实体结构
Structure __SubstInfo
   Flags.w
   NameAddr.l  ;名称地址
   AttiAddr.l  ;属性地址
EndStructure

Structure __MainInfo
   SubstID.i
   EntryID.i
   AttriID.i
   IndexID.i
   TextsID.i
EndStructure
   

Global Dim _DimSubst.l($FFFF)
Global Dim _DimAttri$(1)
Global _Main.__MainInfo

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

;初始化[实体库.bin]
Procedure Init_Subst()
   
   If IsFile(_Main\SubstID) : CloseFile(_Main\SubstID) : EndIf     
   FileID = ReadFile(#PB_Any, #SubstName$)
   If FileID
      Header.__HeaderInfo
      ReadSize = SizeOf(__HeaderInfo)
      ReadData(FileID, @Header, ReadSize)
      If Header\Flags <> #SubstFlags
         MessageRequester("出错提示:1101", "[实体库]文件标志有误!")
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      
      If Header\Version <> 1.00
         MessageRequester("出错提示:1102", "[实体库]文件版本有误!"+StrF(Header\Version))
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      
      If Header\IdxAddr < ReadSize
         MessageRequester("出错提示:1103", "[实体库]文件索引有误!")
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf       
      FileSeek(FileID, Header\IdxAddr)
      ReadData(FileID, @_DimSubst(), $40000)
      _Main\SubstID = FileID
      ProcedureReturn #Null
   Else 
      MessageRequester("出错提示:1104", "[实体库]文件无法打开!")
      ProcedureReturn FileID 
   EndIf 
EndProcedure

;初始化[词条库.bin]
Procedure Init_Entry()
   If IsFile(_Main\EntryID) : CloseFile(_Main\EntryID) : EndIf     
   FileID = ReadFile(#PB_Any, #EntryName$)
   If FileID
      Header.__HeaderInfo
      ReadSize = SizeOf(__HeaderInfo)
      ReadData(FileID, @Header, ReadSize)
      If Header\Flags <> #EntryFlags
         MessageRequester("出错提示:1201", "[词条库]文件标志有误!")
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      
      If Header\Version <> 1.00
         MessageRequester("出错提示:1202", "[词条库]文件版本有误!"+StrF(Header\Version))
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      _Main\EntryID = FileID
      ProcedureReturn #Null
   Else 
      MessageRequester("出错提示:1203", "[词条库]文件无法打开!")
      ProcedureReturn FileID 
   EndIf 
EndProcedure

;初始化[属性库.bin]
Procedure Init_Attri()
   If IsFile(_Main\AttriID) : CloseFile(_Main\AttriID) : EndIf     
   FileID = ReadFile(#PB_Any, #AttriName$)
   If FileID
      FileSize = FileSize(#AttriName$)
      *MemData = AllocateMemory(FileSize)
      ReadData(FileID, *MemData, FileSize)      
      *pHeader.__HeaderInfo = *MemData
      If *pHeader\Flags <> #AttriFlags
         MessageRequester("出错提示:1301", "[属性库]文件标志有误!")
         CloseFile(FileID)
         FreeMemory(*MemData)
         ProcedureReturn #Null
      EndIf 
      
      If *pHeader\Version <> 1.00
         MessageRequester("出错提示:1302", "[属性库]文件版本有误!"+StrF(*pHeader\Version))
         CloseFile(FileID)
         FreeMemory(*MemData)
         ProcedureReturn #Null
      EndIf 
      ReDim _DimAttri$(*pHeader\IdxCount)
      Pos = *pHeader\IdxAddr
      For k = 0 To *pHeader\IdxCount-1
               Address = PeekL(*MemData+Pos)                   : Pos+4
         _DimAttri$(k) = PeekS(*MemData+Address, -1, #PB_UTF8)
      Next 
      FreeMemory(*MemData)
      _Main\AttriID = FileID
      ProcedureReturn #Null
   Else 
      MessageRequester("出错提示:1303", "[属性库]文件无法打开!")
      ProcedureReturn FileID 
   EndIf 
EndProcedure

;初始化[索引库.bin]
Procedure Init_Index()
   If IsFile(_Main\IndexID) : CloseFile(_Main\IndexID) : EndIf     
   FileID = ReadFile(#PB_Any, #IndexName$)
   If FileID
      Header.__HeaderInfo
      ReadSize = SizeOf(__HeaderInfo)
      ReadData(FileID, @Header, ReadSize)
      If Header\Flags <> #IndexFlags
         MessageRequester("出错提示:1401", "[索引库]文件标志有误!")
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      
      If Header\Version <> 1.00
         MessageRequester("出错提示:1402", "[索引库]文件版本有误!"+StrF(Header\Version))
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      _Main\IndexID = FileID
      ProcedureReturn #Null
   Else 
      MessageRequester("出错提示:1403", "[索引库]文件无法打开!")
      ProcedureReturn FileID 
   EndIf 
EndProcedure

;初始化[文本库.bin]
Procedure Init_Texts()
   If IsFile(_Main\TextsID) : CloseFile(_Main\TextsID) : EndIf     
   FileID = ReadFile(#PB_Any, #TextsName$)
   If FileID
      Header.__HeaderInfo
      ReadSize = SizeOf(__HeaderInfo)
      ReadData(FileID, @Header, ReadSize)
      If Header\Flags <> #TextsFlags
         MessageRequester("出错提示:1501", "[文本库]文件标志有误!")
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      
      If Header\Version <> 1.00
         MessageRequester("出错提示:1502", "[文本库]文件版本有误!"+StrF(Header\Version))
         CloseFile(FileID)
         ProcedureReturn #Null
      EndIf 
      _Main\TextsID = FileID
      ProcedureReturn #Null
   Else 
      MessageRequester("出错提示:1503", "[文本库]文件无法打开!")
      ProcedureReturn FileID 
   EndIf 
EndProcedure

;匹配目标
Procedure Find_Target(FindText$)
   If IsFile(_Main\SubstID) = #Null : ProcedureReturn : EndIf 
   If IsFile(_Main\EntryID) = #Null : ProcedureReturn : EndIf 
   If IsFile(_Main\AttriID) = #Null : ProcedureReturn : EndIf 
   If IsFile(_Main\IndexID) = #Null : ProcedureReturn : EndIf 
   If IsFile(_Main\TextsID) = #Null : ProcedureReturn : EndIf 
   
   Length = StringByteLength(FindText$, #PB_Unicode)
   CRC32 = Cipher_CRC32(@FindText$, Length)  ;进行CRC32计算
   Index.l = CRC32 >> 16 & $FFFF             ;获取高16位为索引
   Flags.l = CRC32 & $FFFF                   ;获取低16位为标志
   Address = _DimSubst(Index)                ;获取高16位数组指向的地址
   
   FileSeek(_Main\SubstID, Address)          ;获取相同高16位索引号的数组成员
   Count = ReadWord(_Main\SubstID)           ;获取数组成员数量
   
   Dim DimTemp.__SubstInfo(Count)
   ReadData(_Main\SubstID, @DimTemp(), Count * 10)    ;读取成员信息
   For k = 0 To Count-1
      If Flags = DimTemp(k)\Flags & $FFFF             ;如果低16位为标志同相,则读取实体名称比对
;          Debug "Flags = 0x" + Hex(DimTemp(k)\Flags, #PB_Word) + " : 0x" + Hex(DimTemp(k)\NameAddr, #PB_Long)
         FileSeek(_Main\EntryID, DimTemp(k)\NameAddr) 
         Result$ = ReadString(_Main\EntryID, #PB_UTF8)
         If FindText$ = Result$                       ;如果实体名称一致,则视为找到匹配目标
            IsFind = #True
            Debug "找到了目标:  " + FindText$
            Debug "NameAddr = 0x" + Hex(DimTemp(k)\NameAddr)
            Debug "AttiAddr = 0x" + Hex(DimTemp(k)\AttiAddr)
            ShowText$ = "关键词: " + FindText$ + #CRLF$
            Debug "关键词: " + FindText$ 
            
            FileSeek(_Main\IndexID, DimTemp(k)\AttiAddr)    ;跳到属性索引组
            CountAttri = ReadWord(_Main\IndexID)            ;枚举出所有的属性索引组
            For i = 0 To CountAttri-1
               Attri.q    = ReadQuad(_Main\IndexID)
               Index      = Attri >> 40 & $FFFFFF           ;40-63位为属性索引号
               TextAddr.q = Attri & $00FFFFFFFFFF           ;00-39位为属性值地址
               
               FileSeek(_Main\TextsID, TextAddr) 
               Notes$ = ReadString(_Main\TextsID, #PB_UTF8)
               Debug "AttriIndex = " + Str(Index) + ": 0x" + Hex(TextAddr)
               ShowText$ + "【"+_DimAttri$(Index)+"】"+Notes$ + #CRLF$
               Debug "【"+_DimAttri$(Index)+"】"+Notes$
            Next 
;             SetGadgetText(#rtxResult, ShowText$)
         EndIf 
      EndIf 
   Next 
EndProcedure


;主程
;-============ 

Init_Subst()   ;初始化[实体库.bin]
Init_Entry()   ;初始化[词条库.bin]
Init_Attri()   ;初始化[属性库.bin]
Init_Index()   ;初始化[索引库.bin]
Init_Texts()   ;初始化[文本库.bin]

NewList ListText$()
FileID = ReadFile(#PB_Any, "词条样本.txt") 
If FileID
   Format = ReadStringFormat(FileID)
   Format = #PB_UTF8
   While Eof(FileID) = 0
      LineText$ = ReadString(FileID, Format)
      AddElement(ListText$()) : ListText$() = LineText$
   Wend
EndIf 

TickTime.q = GetTickCount_()
ForEach ListText$()
   Find_Target(ListText$())
Next 
TickTime = GetTickCount_()-TickTime

If IsFile(_Main\SubstID) : CloseFile(_Main\SubstID) : EndIf  
If IsFile(_Main\EntryID) : CloseFile(_Main\EntryID) : EndIf  
If IsFile(_Main\AttriID) : CloseFile(_Main\AttriID) : EndIf  
If IsFile(_Main\IndexID) : CloseFile(_Main\IndexID) : EndIf  
If IsFile(_Main\TextsID) : CloseFile(_Main\TextsID) : EndIf 

Text$ = "查询 " + Str(ListSize(ListText$()))+" 次,共耗时: "+ Str(TickTime)+"毫秒"
Debug Text$
MessageRequester("查询速度测试", Text$)














; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 18
; Folding = Ag
; EnableXP
; Executable = 查询速度测试.exe