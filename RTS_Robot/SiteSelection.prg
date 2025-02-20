#include "RTS_tools.inc"

Function SelectSite
	Integer fileNum
	String SITE_FILE$
	SITE_FILE$ = PROJ_DIR + "site.csv"
	
	If Not FileExists(SITE_FILE$) Then
		Print "***ERROR NO SITE SELECTION FILE"
		Exit Function
	EndIf
	fileNum = FreeFile
	ROpen SITE_FILE$ As #fileNum
	Input SITE$, CHIPTYPE$
	POINTS_FILE$ = PROJ_DIR + "\RTS_Robot\points_" + SITE$ + ".pts"
	LoadPoints POINTS_FILE$
	Select SITE$
		Case "BNL"
			HAND_U0 = 4.174
			DF_CAMERA_OFFSET = 58.
			DF_CAMERA_FOCUS = -18.045
		Case "MSU"
			HAND_U0 = 4.00
			DF_CAMERA_OFFSET = 60.
			DF_CAMERA_FOCUS = -20.
	Send
	
'	Select CHIPTYPE
'		Case "LArASIC"
'			
'		Case "ColdADC"
'			
'		Case "ColDATA"
'			
'		Case "All"
'			
'	Send
	Close #fileNum
Fend

