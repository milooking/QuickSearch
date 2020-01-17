;***********************************
;开 发 者: 迷路仟 QQ:714095563 
;开发时间: 2020.01.17
;编 译 成: 无
;***********************************
;【作用】:用于整理原始文件["中文智能图谱数据ownthink_v2.csv"],
;原始文件数据零乱,整理错乱的数据可能原因,为后面的预处理做基础.
;顺便整理出[属性.ini]和[实体.ini]
;有带界面,EXE模式下，整理一次需要36小时

;实体：1520 7500条
;属性：  43 8656条   其中出现10次或10次以上的　32006条

Structure __AttiInfo
   Count.l   
   Text$
   Line$
EndStructure


; 沙发[软件家具],单位,"""套"",“张”,“个”"
;['词条', '描述', '词条（拼音：cí tiáo）也叫词目，是辞书学用语，指收列的词语及其释文。']
#winScreen = 0
#btnScreen = 1
#lblScreen1 = 2
#lblScreen2 = 3
#AttiText$ = "实体类"


; #FileName$ = "Test.txt"
; #FileName$ = "中文智能图谱数据.txt"
FileName$ = "中文智能图谱数据ownthink_v2.csv"

; 词条{实体{名，类}，属性{下标{值}}}
Global _MainThread 
Global _PrevAtti$

Global NewList _ListName.__AttiInfo()
Global NewMap _MapName()

Global NewList _ListAtti.__AttiInfo()
Global NewMap _MapAtti()




Procedure Thread_Parser(LineText$)
   
   ;============
   ;记录属性
   _MapAtti(#AttiText$)

   ; ===========　分析实体
   Index = 1 : CountParser = CountString(LineText$, ",")+1
   ;获取第一段的内容，
   NameText$ = StringField(LineText$, Index, ",")     
   ;判断内容是否为空
   NameText$ = ReplaceString(NameText$, " ", "")
   NameText$ = ReplaceString(NameText$, "　", "")
   If NameText$ = #Null$ : ProcedureReturn : EndIf 
   
   ;判断内容是否包含双引号
   Count = CountString(NameText$, #DQUOTE$)
   If Count 
      ;判断双引号是不是在第一个字节
      If Left(NameText$, 1) = #DQUOTE$
         NameText$ = Mid(NameText$, 2)
         NameText$ = ReplaceString(NameText$, #DQUOTE$+#DQUOTE$, "@")
         NameCount = CountString(NameText$, #DQUOTE$)+1
         While NameCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            NameText$ = NameText$ +","+ PartText$
            NameCount = CountString(NameText$, #DQUOTE$)+1
         Wend 
         
         NameText$ = Mid(NameText$, 1, Len(NameText$)-1)
         NameText$ = ReplaceString(NameText$, "@", #DQUOTE$)
      Else 
         NameText$ = ReplaceString(NameText$, #DQUOTE$+#DQUOTE$, "@")
         NameCount = CountString(NameText$, #DQUOTE$)+0
         While NameCount % 2 = 1 And Index <= CountParser
            Index+1
            PartText$ = StringField(LineText$, Index, ",")
            PartText$ = ReplaceString(PartText$, " ", "")
            PartText$ = ReplaceString(PartText$, "　", "")
            PartText$ = ReplaceString(PartText$, #DQUOTE$+#DQUOTE$, "@")
            NameText$ = NameText$ +","+ PartText$
            NameCount = CountString(NameText$, #DQUOTE$)+0
         Wend 
         NameText$ = ReplaceString(NameText$, "@", #DQUOTE$)
      EndIf 
   EndIf 
   NoteText$ = #Null$
   If FindString(NameText$, "[")
      NoteText$ = StringField(NameText$, 2, "[")
      NoteText$ = Mid(NoteText$, 1, Len(NoteText$)-1)
      NameText$ = StringField(NameText$, 1, "[")
      If NameText$ = NoteText$
         NoteText$ =  #Null$
      EndIf 
   EndIf 
   
   If NoteText$ = #Null$
;       Debug "实体 = "+NameText$ + " "  
   Else       
;       Debug "【实体】"+NameText$ + " 【属性】实体类 【值】" + NoteText$
;       Debug "实体 = "+NameText$ + " 属性 = " + AttiText$
   EndIf 
   ;============
   ;记录实体
   If FindMapElement(_MapName(), NameText$) = 0
      AddElement(_ListName())
      _ListName()\Text$ = NameText$
      _MapName(NameText$)
      _MapAtti(#AttiText$)
      _MapAtti()+1
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
      Case "①","②","③","④","⑤","⑥","⑦","⑧","⑨","⑩"  　: AttiText$ = Mid(AttiText$, 1, Len(AttiText$)-1)
   EndSelect
   
   Select Left(AttiText$, 1) 
      Case "*", "."                                            : AttiText$ = Mid(AttiText$, 2)
   EndSelect   
   
   If AttiText$ = "." :  AttiText$ = _PrevAtti$ : EndIf 
   _PrevAtti$ = AttiText$
   ;    Debug "实体 = "+NameText$ + " 属性 = " + AttiText$
   ;============
   ;记录属性
   _MapAtti(AttiText$)
   _MapAtti()+1 
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
   Debug "【实体】"+NameText$ + " 【属性】" + AttiText$+ " 【值】" + NoteText$
EndProcedure

Procedure Thread(Index)
   
   ClearList(_ListName())
   ClearList(_ListAtti())
   ClearMap(_MapName())
   ClearMap(_MapAtti())
   
   StartTimer = mcsGetTime_()
   
   FileID = ReadFile(#PB_Any, #FileName$) 
   If FileID
      Format = ReadStringFormat(FileID)
      Format = #PB_UTF8
      While Eof(FileID) = 0 And _MainThread = #True
         LineText$ = ReadString(FileID, Format)
         ;===========　加载这完整的文本
         Count = CountString(LineText$, #DQUOTE$)
         If Count 
            While Count % 2 = 1
               LineText$ = LineText$ +"\n"+ ReadString(FileID, Format)
               Count = CountString(LineText$, #DQUOTE$)
            Wend            
         EndIf 
         Thread_Parser(LineText$)
         Index+1
         If Index % 1000 = 0
            SetGadgetText(#lblScreen1, Str(Index)) 
         EndIf 
         CurrTimer = mcsGetTime_(StartTimer)
         If CurrTimer > NextTimer 
            SetGadgetText(#lblScreen2, FormatDate("%hh:%ii:%ss", NextTimer/1000)) 
            NextTimer = NextTimer+1000
         EndIf          
       Wend 
      CloseFile(FileID)
   EndIf    
   SetGadgetText(#lblScreen1, Str(Index)) 
;    Delay(10)
   ;===========创建【属性.txt】
   ForEach _MapAtti()
      AddElement(_ListAtti())
      _ListAtti()\Count = _MapAtti()
      _ListAtti()\Text$ = MapKey(_MapAtti())
   Next 
   SortStructuredList(_ListAtti(), 1, 0, #PB_Long)
   If CreateFile(0, "属性.ini")
      ForEach _ListAtti()
         WriteStringN(0, "【"+_ListAtti()\Text$+"】"+Str(_ListAtti()\Count))
      Next 
      CloseFile(0)
   EndIf 
   ;===========创建【实体.txt】
   If CreateFile(1, "实体.ini")
      ForEach _ListName()
         WriteStringN(1, _ListName()\Text$)
      Next 
      CloseFile(1)
   EndIf 
   
   MessageRequester("", "完成")

EndProcedure

WindowFlags = #PB_Window_ScreenCentered| #PB_Window_SystemMenu| #PB_Window_MinimizeGadget|
              #PB_Window_MaximizeGadget| #PB_Window_SizeGadget 
hWindow = OpenWindow(#winScreen, 0, 0, 400, 250, "整理程序", WindowFlags)

TextGadget  (#lblScreen1, 000, 100, 400, 020, #FileName$, #PB_Text_Center)
TextGadget  (#lblScreen2, 000, 050, 400, 020, #FileName$, #PB_Text_Center)
ButtonGadget(#btnScreen, 150, 150, 100, 050, "开始", #PB_Button_Toggle)

Repeat
   WinEvent  = WindowEvent()
   Select WinEvent
      Case #PB_Event_CloseWindow       
         IsExitWindow = #True
      Case #PB_Event_Gadget
         If EventGadget() = #btnScreen
            If GetGadgetState(#btnScreen) = #False
               _MainThread = #False
               SetGadgetText(#btnScreen, "开始") 
            Else 
               _MainThread = #True
               SetGadgetText(#btnScreen, "停止")
               CreateThread(@Thread(), 0)
            EndIf 
         EndIf 
   EndSelect      
   Delay(1)
Until IsExitWindow = #True

End







; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 12
; Folding = 0
; EnableXP
; Executable = dddddd.exe