#include "RTS_tools.inc"

Function SelectSite
	Integer fileNum
	String SITE_FILE$
	SITE_FILE$ = PROJ_DIR + "\site.csv"
	
	If Not FileExists(SITE_FILE$) Then
		Print "***ERROR NO SITE SELECTION FILE"
		Exit Function
	EndIf
	fileNum = FreeFile
	ROpen SITE_FILE$ As #fileNum
	Input #fileNum, SITE$, CHIPTYPE$
	POINTS_FILE$ = "points_" + SITE$ + ".pts"
	Print "Site selected is " + SITE$
	Print "Chip type to be tested is" + CHIPTYPE$
	LoadPoints POINTS_FILE$
	Select SITE$
		Case "BNL"
			HAND_U0 = 4.174
			DF_CAMERA_OFFSET = 58.
			DF_CAMERA_FOCUS = -18.045
		Case "MSU"
			HAND_U0 = -0.947
			DF_CAMERA_OFFSET = 57.961
			DF_CAMERA_FOCUS = -19.616
	Send
	
	Close #fileNum
Fend

