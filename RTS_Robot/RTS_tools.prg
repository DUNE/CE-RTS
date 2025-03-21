#include "RTS_tools.inc"

Function PumpOn
    VacuumValveOpen
    Wait 1
    VacuumValveClose
	On 9
Fend

Function PumpOff
	Off 9
Fend

Function VacuumValveOpen
	On 10
Fend

Function VacuumValveClose
	Off 10
Fend

Function PlungerOn
	On 11
Fend

Function PlungerOff
	Off 11
Fend

Function isVacuumOk As Boolean
	If Sw(10) = 0 Then
		isVacuumOk = True
	Else
		Print "Bad vacuum"
		isVacuumOk = False
	EndIf
Fend

Function isPressureOk As Boolean
	If Sw(11) Then
		isPressureOk = True
	Else
		Print "Bad pressure"
		isPressureOk = False
	EndIf
Fend

Function isContactSensorTouches As Boolean
	If Sw(8) = 1 Then
		isContactSensorTouches = True
	Else
		isContactSensorTouches = False
	EndIf
	
	' Lost power case: assume that the tools is touching:
	If Sw(8) = Sw(9) Then
		isContactSensorTouches = True
	EndIf
Fend

Function SetSpeed
	Power Low
	Speed 100
	Accel 10, 10
	Speed 1
	Accel 1, 1
Fend


Function MoveFromPointToImage(dU As Double, RotateFirst As Boolean)
	' Move arm from stinger at point, to chip in focus with some rotation in degrees
	' Remember point is defined as some offset (10mm from contact)
	' RotateFirst decides if rotation comes before or after translation in XY
	' Which may be important to avoid collision
	
	If (DF_CAM_Z_OFF < 0) Then
		Print "ERROR - Z OFFSET IS NEGATIVE OR NOT SET - SET CAMERA OFFSETS USING DEFINED PROCEDURE ON GIT WIKI"
		Exit Function
	EndIf
	
	Move Here +Z(DF_CAM_Z_OFF)
	If RotateFirst Then
		Go Here +U(dU)
		Move Here +X(XOffset(CU(Here))) +Y(YOffset(CU(Here)))
	Else
		Move Here +X(XOffset(CU(Here) + dU)) +Y(YOffset(CU(Here) + dU))
		Go Here +U(dU)
	EndIf
	
Fend

Function MoveFromImageToPoint(dU As Double, RotateFirst As Boolean)
	' Inverse of above function, note rotation is not inverted like other offsets
	' And order of operations may need to be reversed if this matters for collisions
	' (Rotations should happen in same XY position)
	' e.g. MoveFromImageToPoint(-45,1) is inverse of MoveFromPointToImage(45,0)
	If (DF_CAM_Z_OFF < 0) Then
		Print "ERROR - Z OFFSET IS NEGATIVE OR NOT SET - SET CAMERA OFFSETS USING DEFINED PROCEDURE ON GIT WIKI"
		Exit Function
	EndIf
	
	If RotateFirst Then
		Go Here +U(dU)
		Move Here -X(XOffset(CU(Here) - dU)) -Y(YOffset(CU(Here) - dU))
	Else
		Move Here -X(XOffset(CU(Here))) -Y(YOffset(CU(Here)))
		Go Here +U(dU)
	EndIf
	Move Here -Z(DF_CAM_Z_OFF) Till Sw(8) = On Or Sw(9) = Off
	If Sw(8) = On Or Sw(9) = Off Then
		Print "ERROR: Contact was made. point should be defined above the chip! Check for obstructions"
	EndIf
	
Fend

' Jump to camera
' Preserve U rotation
Function JumpToCamera
	
	If Agl(2) < 0 Then
		' Left-handed orientation
    	Jump P_camera :U(CU(Here)) /L
    Else
    	' Right-handed orientation 
    	Jump P_camera :U(CU(Here)) /R
	EndIf
	
Fend

' pallet_nr 1..2 (1-left, 2-right)
' row_nr = 1..6
' col_nr = 1..15
Function JumpToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer)
	Jump Pallet(pallet_nr, col_nr, row_nr) ' +Z(10)
Fend

' pallet_nr 1..2 (1-left, 2-right)
' row_nr = 1..6
' col_nr = 1..15
Function JumpToTray_camera(pallet_nr As Integer, col_nr As Integer, row_nr As Integer)
	If pallet_nr = 1 Then
'		Jump Pallet(pallet_nr, col_nr, row_nr) +X(DF_CAMERA_OFFSET) :Z(DF_CAMERA_FOCUS) :U(HAND_U0 + 360)
		Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0)) +Y(YOffset(HAND_U0)) +Z(DF_CAM_Z_OFF) :U(HAND_U0)
	ElseIf pallet_nr = 2 Then
'		Jump Pallet(pallet_nr, col_nr, row_nr) -X(DF_CAMERA_OFFSET) :Z(DF_CAMERA_FOCUS) :U(HAND_U0 + 180)
		Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0 + 180)) +Y(YOffset(HAND_U0 + 180)) +Z(DF_CAM_Z_OFF) :U(HAND_U0 + 180)
	EndIf
Fend

Function TouchChip As Byte
	' Will put the stinger in contact with a chip		
	' This does several safety checks	
	' Returns code:
	' 0  - Success
	' -1 - No contact after travelling 2mm below expected position 
	' -2 - No contact but still stopped within +/- 2mm for some reason
	' +1 - Made contact 2mm above expected chip position	
	Speed 1
	Accel 1, 1
	Double Zexpect, Znow, Zdiff
	Zexpect = CZ(Here) - 10.
    Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off
    Wait 0.5
    Znow = CZ(Here)
    Zdiff = Znow - Zexpect
    If Zdiff > 1. Then
    	Print "***ERROR: Contact too early - Check for obstruction"
    	TouchChip = 1
    ElseIf Zdiff < -1. Then
    	Print "***ERROR: No contact - chip missing?"
    	TouchChip = -1
    ElseIf (Not isContactSensorTouches) Then
    	Print "***ERROR: Contact not made but stopped in +/- 2mm of expected"
    	TouchChip = -2
    Else
    	TouchChip = 0
	EndIf
	SetSpeed
Fend

Function isChipInTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer) As Boolean
	JumpToTray(pallet_nr, col_nr, row_nr)
'	Speed 1
'	Accel 1, 1
'    Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off
'    Wait 0.5
'	Boolean TouchStatus
'	TouchStatus = TouchChip
	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	isChipInTray = TouchSuccess
'	SetSpeed
Fend

Function PickupFromTray As Boolean
	
	PickupFromTray = False
	Speed 1
	Accel 1, 1
	' Test if the pickup tool is touches - it should not
	If isContactSensorTouches Then
		Print "ERROR! Contact Sensor is ON"
		Exit Function
	EndIf
	If Not isVacuumOk Then
		Exit Function
	EndIf
'    Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off
'	If Not isContactSensorTouches Then
'		Print "ERROR! Contact sensor does not detect a chip in the tray"
'		Exit Function
'	EndIf
	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	If Not TouchSuccess Then
		Print "ERROR! Cannot pick up from tray"
		Exit Function
	EndIf
	
	VacuumValveOpen
	Go Here +Z(10)
    SetSpeed
    PickupFromTray = True
Fend

Function DropToTray As Boolean
	
	DropToTray = False
	Speed 1
	Accel 1, 1
    Go Here -Z(9) Till Sw(8) = On Or Sw(9) = Off
	If isContactSensorTouches Then
	' XN			
		'Go Here +Z(9) +Y(0.1)
		'Go Here -Z(9)
		'If isContactSensorTouches Then
			
		'EndIf
	' XN end
	
		Print "ERROR! Contact Sensor detects obstacle in the tray"
		Exit Function
	EndIf
	
	
	
    Go Here -Z(1)
    Wait 0.5
	If Not isContactSensorTouches Then
		Print "ERROR! Contact Sensor does not detect contact with the chip"
		Exit Function
	EndIf
	VacuumValveClose
    Wait 2
    Go Here +Z(10)
    SetSpeed
    DropToTray = True
Fend


Function JumpToSocket(DAT_nr As Integer, socket_nr As Integer)
	If DAT_nr = 2 Then
        'Jump P(200 + socket_nr) :Z(-132.5)
        Jump P(200 + socket_nr)
		Print P(200 + socket_nr)
	ElseIf DAT_nr = 1 Then
		'Jump P(100 + socket_nr) -X(DF_CAMERA_OFFSET) :Z(-134.682)
		Jump P(100 + socket_nr)
		'Jump P(100 + socket_nr) -X(DF_CAMERA_OFFSET) -U(135) :Z(-100.682)
		Print P(100 + socket_nr)
	EndIf
	
Fend

Function JumpToSocket_cor(DAT_nr As Integer, socket_nr As Integer)
	'Print
	'Print socket_nr, "**********************************************"
	
	JumpToSocket_camera(DAT_nr, socket_nr)
	
	VRun skt_cali_test
	
	Boolean Isfound1, Isfound2, Isfound3
	Boolean found
	
	Double x_p1, y_p1, a_p1, x_p2, y_p2, a_p2, x_p3, y_p3, a_p3
	Double x_ori, y_ori, a_ori
	
	'VGet skt_cali_test.CameraCenter.RobotXYU, found, x_ori, y_ori, a_ori
	Double check
	check = 100
	Integer N_round
	N_round = 0
	
	Do Until check < 20 And check > -20 Or N_round > 10
		VRun skt_cali_test
		VGet skt_cali_test.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		'Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		'Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		'Print "P3 xyu: ", x_p3, y_p3, a_p3
	
		check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
		N_round = N_round + 1
		'Print "perpendicular check: ", check, " Loop: ", N_round
	
	Loop
	
	
	If check < 20 And check > -20 Then
		Print "Correctly found"
		Double x_c, y_c
		x_c = (x_p1 + x_p3) /2
		y_c = (y_p1 + y_p3) /2

		Print "corr_center: ", x_c, y_c
		JumpToSocket(DAT_nr, socket_nr)
		Jump Here :X(x_c) :Y(y_c)
		
	EndIf
	
	
	
Fend

Function isChipInSocket(DAT_nr As Integer, socket_nr As Integer) As Boolean
	JumpToSocket(DAT_nr, socket_nr)
'	Speed 1
'	Accel 1, 1
'    Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off
'    Wait 0.5
'	isChipInSocket = isContactSensorTouches
	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	isChipInSocket = TouchSuccess
    SetSpeed
Fend


Function InsertIntoSocket As Boolean

	InsertIntoSocket = False

	If Not isPressureOk Then
		Exit Function
	EndIf

	Speed 1
	Accel 1, 1
	PlungerOn
'	Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off	
	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	If Not TouchSuccess Then
		Print "ERROR! Cannot insert in socket"
		Exit Function
	EndIf
	
	VacuumValveClose
	Wait 2
	InsertIntoSocket = isContactSensorTouches
	Go Here +Z(20)
	PlungerOff
	SetSpeed
	
Fend


Function PickupFromSocket As Boolean

	PickupFromSocket = False

	If Not isVacuumOk Then
		Exit Function
	EndIf

	If Not isPressureOk Then
		Exit Function
	EndIf


	Speed 1
	Accel 1, 1

	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	If Not TouchSuccess Then
		Print "ERROR! Cannot pick up from socket"
		Exit Function
	EndIf
	
	'Go Here +U(45)
'	Go Here -Z(12) Till Sw(8) = On Or Sw(9) = Off
	Wait 0.5
'	If Not isContactSensorTouches Then
'		Print "***ERROR! Do not detect chip in the socket"
'		Exit Function
'	EndIf
	Wait 1
	VacuumValveOpen
	Wait 1
	PlungerOn
    Wait 1
    Go Here +Z(20)
    PlungerOff
    SetSpeed

	PickupFromSocket = True
    
Fend

Function UF_camera_light_ON
	On 12
Fend

Function UF_camera_light_OFF
	Off 12
Fend

Function JumpToSocket_camera(DAT_nr As Integer, socket_nr As Integer)
	If DAT_nr = 2 Then
'		Jump P(200 + socket_nr) :Z(-97.60) +X(DF_CAMERA_OFFSET) -U(45)
		Jump P(200 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here) - 45)) +Y(YOffset(CU(Here) - 45)) -U(45)
	ElseIf DAT_nr = 1 Then
		'Jump P(100 + socket_nr) +U(135)
		Jump P(100 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here) + 135)) +Y(YOffset(CU(Here) + 135)) +U(135)
	EndIf
Fend

Function UF_take_picture$(basename$ As String) As String
 	UF_take_picture$ = RTS_DATA$ + "\images\UF_" + basename$ + ".bmp"
 	Print UF_take_picture$
 	VRun UF
    Wait 0.3
	VSaveImage UF, UF_take_picture$
Fend


Function DF_take_picture$(basename$ As String) As String
	DF_take_picture$ = RTS_DATA$ + "\images\" + basename$ + ".bmp"
	Print DF_take_picture$
 	VRun DF
    Wait 0.3
	VSaveImage DF, DF_take_picture$
Fend



' ARGUMENTS
' INPUT: 
'       id$ - operation id (timestamp)
'       idx(20) - array of indexes
'       idx(1)  - CSV file index
'       idx(2)  - pallet number source (1-left, 2-right, 0 - N/A)
'       idx(3)  - pallet column number source (1-15, 0 - N/A)
'       idx(4)  - pallet row    number source (1-6 , 0 - N/A)
'       idx(5)  - pallet number target (1-left, 2-right, 0 - N/A)
'       idx(6)  - pallet column number target (1-15, 0 - N/A)
'       idx(7)  - pallet row    number target (1-6 , 0 - N/A)
'       idx(8)  - DAT board number source (1 - left, 2 - right, 0 - N/A)
'       idx(9)  - socket number source (1-8, 0 - N/A)
'       idx(10) - DAT board number target (1 - left, 2 - right, 0 - N/A)
'       idx(11) - socket number target (1-8, 0 - N/A)
'
' OUTPUT:
'       status - 0 - success, > 0 - error number
'       res(30) - array of results
' Results of analysis of chip position as it came from the source:
'       res(1)  - camera X, [mm]
'       res(2)  - camera Y, [mm]
'       res(3)  - chip X, initial measurement, [mm]
'       res(4)  - chip Y, initial measurement, [mm]
'       res(5)  - chip rotation, initial measurement, [deg]
'       res(6)  - chip X, measurement of the chip rotated by 180 deg, [mm]
'       res(7)  - chip Y, measurement of the chip rotated by 180 deg, [mm]
'       res(8)  - chip rotation, measurement of the chip rotated by 180 deg, [deg]
'       res(9)  - tool X [mm]
'       res(10) - tool Y [mm]
'       res(11) - chip X position relative to the tool [mm]
'       res(12) - chip Y position relative to the tool [mm]
'
' Results of analysis of chip position for destination:      
'       res(13) - chip X with hand at target rotation [mm]
'       res(14) - chip Y with hand at target rotation [mm]
'       res(15) - chip rotation with hand at target rotation [deg]
'
'       res(16) - dU - correction for chip rotation [deg]
'       res(17) - chip X, measurement at 0 deg, corrected for dU, [mm]
'       res(18) - chip Y, measurement at 0 deg, corrected for dU, [mm]
'       res(19) - chip angle, measurement at 0 deg, corrected for dU, [deg]
'       res(20) - dX - chip X correction [mm]
'       res(21) - dY - chip Y correction [mm]
'       rest(22-30) reserved / unused
'
' GLOBAL
'      tray_X
'      tray_Y
'      tray_U
'      DAT_X
'      DAT_Y
'      DAT_U


'Function ChipBottomAnaly(chip_SN$ As String, fileNum As Integer, pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_NR As Integer, ByRef status As Integer, ByRef res() As Double)
'Function ChipBottomAnaly(chip_SN$ As String, ByRef idx() As Integer, ByRef status As Integer, ByRef res() As Double)
Function ChipBottomAnaly(id$ As String, ByRef idx() As Integer, ByRef res() As Double) As Integer

	Integer i, fileNum
	
	ChipBottomAnaly = 0

	' reset the array of results
	For i = 1 To 30
		res(i) = 0
	Next i
	
	
	' sources and targets of the chip
	Integer src_pallet_nr, src_col_nr, src_row_nr, src_DAT_nr, src_socket_nr
	Integer tgt_pallet_nr, tgt_col_nr, tgt_row_nr, tgt_DAT_nr, tgt_socket_nr
	
	fileNum = idx(1)
	src_pallet_nr = idx(2)
	src_col_nr = idx(3)
	src_row_nr = idx(4)
	tgt_pallet_nr = idx(5)
	tgt_col_nr = idx(6)
	tgt_row_nr = idx(7)

	src_DAT_nr = idx(8)
	src_socket_nr = idx(9)
	tgt_DAT_nr = idx(10)
	tgt_socket_nr = idx(11)
			
	'If tgt_DAT_nr = 1 Then
	'	ChipBottomAnaly = 10
	'	Print "***ERROR Functionality for DAT board 1 not implemented yet", 10
    '    Exit Function
	'EndIf
		
	' target position at the camera
	Double tgt_x0, tgt_y0, tgt_u0
	' hand rotation at destination
	Double dst_U
	If tgt_pallet_nr > 0 And tgt_pallet_nr <= NTRAYS And tgt_col_nr > 0 And tgt_col_nr <= TRAY_NCOLS And tgt_row_nr > 0 And tgt_row_nr <= TRAY_NROWS Then
		tgt_x0 = tray_X(tgt_pallet_nr, tgt_col_nr, tgt_row_nr)
		tgt_y0 = tray_Y(tgt_pallet_nr, tgt_col_nr, tgt_row_nr)
		tgt_u0 = tray_U(tgt_pallet_nr, tgt_col_nr, tgt_row_nr)
		dst_U = CU(Pallet(tgt_pallet_nr, tgt_col_nr, tgt_row_nr))
	ElseIf tgt_DAT_nr > 0 And tgt_DAT_nr <= 2 And tgt_socket_nr > 0 And tgt_socket_nr <= NSOCKETS Then
		tgt_x0 = DAT_X(tgt_DAT_nr, tgt_socket_nr)
		tgt_y0 = DAT_Y(tgt_DAT_nr, tgt_socket_nr)
		tgt_u0 = DAT_U(tgt_DAT_nr, tgt_socket_nr)
		dst_U = CU(P(100 * tgt_DAT_nr + tgt_socket_nr))
	Else
		ChipBottomAnaly = 100
		Exit Function
	EndIf
	
	
	       'JW:
       If Agl(4) < -45. Then
               Go Here +U(180)
'      ElseIf Agl(4) < 0. Then
'              Go Here +U(135)
'              Go Here -U(180)
'              Go Here +U(45)
       ElseIf Agl(4) <= 45. Then
               Go Here +U(90)
       Else
               Go Here -U(180)
       EndIf
	
	
	UF_camera_light_ON
	Wait 0.2
	String pict_fname$
	'UF_take_picture(chip_SN$, ByRef pict_fname_0$)
	pict_fname$ = UF_take_picture$(id$ + "_01")
    Print #fileNum, ",", pict_fname$,
	
	'Double tray_dx, tray_dy, tray_dU
	VRun ChipBottom_Analy

	Boolean ret_found
	Double camera_X, camera_Y
	Double X_0, Y_0, U_0
	Double X_180, Y_180, U_180
	Double X_tool, Y_tool

	'VGet ChipBottom_Analy.Final.RobotXYU, ret_found, ret_X, ret_Y, ret_U
	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then

		VGet ChipBottom_Analy.CameraCenter.CameraX, camera_X
		VGet ChipBottom_Analy.CameraCenter.CameraY, camera_Y
			
		VGet ChipBottom_Analy.Final.CameraX, X_0
		VGet ChipBottom_Analy.Final.CameraY, Y_0
		VGet ChipBottom_Analy.Final.Angle, U_0

		Print #fileNum, ",", ret_found,
		Print #fileNum, ",", camera_X, ",", camera_Y,
		Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,
		
		res(1) = camera_X
		res(2) = camera_Y
		
		res(3) = X_0
		res(4) = Y_0
		res(5) = U_0

	Else
		
		ChipBottomAnaly = 1
		Print "***ERROR ", 1
        Exit Function
		
	EndIf

	' Repeat measurement for 180 deg. rotation
'	If CU(Here) < 235 Then
'		Go Here +U(180)
'	Else
'		Go Here -U(180)
'	EndIf

'      JW:
	If Agl(4) <= 45. Then
	    Go Here -U(180)
    Else
        Go Here +U(180)
    EndIf

	Wait 0.2
	'UF_take_picture(chip_SN$ + "-180", ByRef pict_fname_180$)
	'UF_take_picture(ByRef pict_fname$)
	pict_fname$ = UF_take_picture$(id$ + "_02")
	Print #fileNum, ",", pict_fname$,

	VRun ChipBottom_Analy
	
	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then
		
		VGet ChipBottom_Analy.Final.CameraX, X_180
		VGet ChipBottom_Analy.Final.CameraY, Y_180
		VGet ChipBottom_Analy.Final.Angle, U_180

		Print #fileNum, ",", ret_found,
		Print #fileNum, ",", X_180, ",", Y_180, ",", U_180,

		res(6) = X_180
		res(7) = Y_180
		res(8) = U_180

		X_tool = 0.5 * (X_0 + X_180)
		Y_tool = 0.5 * (Y_0 + Y_180)

		res(9) = X_tool
		res(10) = Y_tool

		Print #fileNum, ",", X_tool, ",", Y_tool,

		' chip position relative to the tool
		res(11) = X_0 - X_tool
		res(12) = Y_0 - Y_tool

		Print #fileNum, ",", res(11), ",", res(12),

		' record the chip position from the source
		Double src_x0, src_y0, src_u0
		' source: 1 - pallet, 2 - socket
		Integer src
		If src_pallet_nr > 0 And src_pallet_nr <= NTRAYS And src_col_nr > 0 And src_col_nr <= TRAY_NCOLS And src_row_nr > 0 And src_row_nr <= TRAY_NROWS Then
			src_x0 = tray_X(src_pallet_nr, src_col_nr, src_row_nr)
			src_y0 = tray_Y(src_pallet_nr, src_col_nr, src_row_nr)
			src_u0 = tray_U(src_pallet_nr, src_col_nr, src_row_nr)
			src = 1
		ElseIf src_DAT_nr > 0 And src_DAT_nr <= 2 And src_socket_nr > 0 And src_socket_nr <= NSOCKETS Then
			src_x0 = DAT_X(src_DAT_nr, src_socket_nr)
			src_y0 = DAT_Y(src_DAT_nr, src_socket_nr)
			src_u0 = DAT_U(src_DAT_nr, src_socket_nr)
			src = 2
		Else
			ChipBottomAnaly = 101
			Exit Function
		EndIf

		If src_x0 = 0 And src_y0 = 0 And src_u0 = 0 Then
			Print "Recording position of the source: ", res(11), " ", res(12), " ", U_0
			If src = 1 Then
				tray_X(src_pallet_nr, src_col_nr, src_row_nr) = res(11)
				tray_Y(src_pallet_nr, src_col_nr, src_row_nr) = res(12)
				tray_U(src_pallet_nr, src_col_nr, src_row_nr) = U_0
			ElseIf src = 2 Then
				DAT_X(src_DAT_nr, src_socket_nr) = res(11)
				DAT_Y(src_DAT_nr, src_socket_nr) = res(12)
				DAT_U(src_DAT_nr, src_socket_nr) = U_0
			Else
				ChipBottomAnaly = 102
				Exit Function
			EndIf
		EndIf

	Else

		ChipBottomAnaly = 2
		Print "***Error ", 2
        Exit Function
	EndIf
	
    ' JW: need to correct the cases where you do +90, then -180 then +90
         ' Other angles just do +180, then -180 or vice versa
	If Agl(4) > -45. And Agl(4) <= 45. Then
		Go Here +U(90)
	EndIf


	' Change handeness to the target 
	If tgt_pallet_nr = 1 Or tgt_DAT_nr = 1 Then
    	Jump P_camera :U(dst_U) /R
    ElseIf tgt_pallet_nr = 2 Or tgt_DAT_nr = 2 Then
    	Jump P_camera :U(dst_U) /L
    EndIf
	

	' Rotate the hand to destination orientation Update: Done in a previous step
	'Go Here :U(dst_U)
	Wait 0.2

	' Added 2024-06-18
	' Re-evaluate the position of the tool
	VRun ChipBottom_Analy

	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then

		'VGet ChipBottom_Analy.CameraCenter.CameraX, camera_X
		'VGet ChipBottom_Analy.CameraCenter.CameraY, camera_Y
			
		VGet ChipBottom_Analy.Final.CameraX, X_0
		VGet ChipBottom_Analy.Final.CameraY, Y_0
		VGet ChipBottom_Analy.Final.Angle, U_0

		Print #fileNum, ",", ret_found,
		'Print #fileNum, ",", camera_X, ",", camera_Y,
		Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,
		
		'res(1) = camera_X
		'res(2) = camera_Y
		
		'res(3) = X_0
		'res(4) = Y_0
		'res(5) = U_0

	Else
		
		ChipBottomAnaly = 301
		Print "***ERROR ", 301
        Exit Function
		
	EndIf

	' Repeat measurement for 180 deg. rotation
	'If CU(Here) < 180 Then
	
	'If Hand < 1.5 Then ' 1 for right handed, 2 for left handed.
		Go Here +U(180)
	'Else
	'	Go Here -U(180)
	'EndIf
	Wait 0.2
	'UF_take_picture(chip_SN$ + "-180", ByRef pict_fname_180$)
	'UF_take_picture(ByRef pict_fname$)
	'pict_fname$ = UF_take_picture$(id$ + "_02")
	'Print #fileNum, ",", pict_fname$,

	VRun ChipBottom_Analy
	
	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then
		
		VGet ChipBottom_Analy.Final.CameraX, X_180
		VGet ChipBottom_Analy.Final.CameraY, Y_180
		VGet ChipBottom_Analy.Final.Angle, U_180

		Print #fileNum, ",", ret_found,
		Print #fileNum, ",", X_180, ",", Y_180, ",", U_180,

		'res(6) = X_180
		'res(7) = Y_180
		'res(8) = U_180

		X_tool = 0.5 * (X_0 + X_180)
		Y_tool = 0.5 * (Y_0 + Y_180)

		res(9) = X_tool
		res(10) = Y_tool

		Print #fileNum, ",", X_tool, ",", Y_tool,

		Print "Tool position: X=", X_tool, ", Y=", Y_tool

		' chip position relative to the tool
		res(11) = X_0 - X_tool
		res(12) = Y_0 - Y_tool

		Print #fileNum, ",", res(11), ",", res(12),

		' record the chip position from the source
		'Double src_x0, src_y0, src_u0
		'' source: 1 - pallet, 2 - socket
		'Integer src
		'If src_pallet_nr > 0 And src_pallet_nr <= NTRAYS And src_col_nr > 0 And src_col_nr <= TRAY_NCOLS And src_row_nr > 0 And src_row_nr <= TRAY_NROWS Then
		'	src_x0 = tray_X(src_pallet_nr, src_col_nr, src_row_nr)
		'	src_y0 = tray_Y(src_pallet_nr, src_col_nr, src_row_nr)
		'	src_u0 = tray_U(src_pallet_nr, src_col_nr, src_row_nr)
		'	src = 1
		'ElseIf src_DAT_nr > 0 And src_DAT_nr <= 2 And src_socket_nr > 0 And src_socket_nr <= NSOCKETS Then
	'		src_x0 = DAT_X(src_DAT_nr, src_socket_nr)
	'		src_y0 = DAT_Y(src_DAT_nr, src_socket_nr)
	'		src_u0 = DAT_U(src_DAT_nr, src_socket_nr)
	'		src = 2
	'	Else
	'		ChipBottomAnaly = 101
	'		Exit Function
	'	EndIf

	'	If src_x0 = 0 And src_y0 = 0 And src_u0 = 0 Then
	'		Print "Recording position of the source: ", res(11), " ", res(12), " ", U_0
	'		If src = 1 Then
	'			tray_X(src_pallet_nr, src_col_nr, src_row_nr) = res(11)
	'			tray_Y(src_pallet_nr, src_col_nr, src_row_nr) = res(12)
	'			tray_U(src_pallet_nr, src_col_nr, src_row_nr) = U_0
	'		ElseIf src = 2 Then
	'			DAT_X(src_DAT_nr, src_socket_nr) = res(11)
	'			DAT_Y(src_DAT_nr, src_socket_nr) = res(12)
	'			DAT_U(src_DAT_nr, src_socket_nr) = U_0
	'		Else
	'			ChipBottomAnaly = 102
	'			Exit Function
	'		EndIf
	'	EndIf

	Else

		ChipBottomAnaly = 302
		Print "***Error ", 302
        Exit Function
			
	EndIf


	' End of code added on 2024-06-18

	' re-evaluate chip position with the rotation at destination
	Go Here :U(dst_U)
	Wait 0.2

	'UF_take_picture(ByRef pict_fname$)
	pict_fname$ = UF_take_picture$(id$ + "_03")
	Print #fileNum, ",", pict_fname$,

	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then
			
		VGet ChipBottom_Analy.Final.CameraX, X_0
		VGet ChipBottom_Analy.Final.CameraY, Y_0
		VGet ChipBottom_Analy.Final.Angle, U_0

		Print #fileNum, ",", ret_found,
		Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,
				
		res(13) = X_0
		res(14) = Y_0
		res(15) = U_0

	Else
		
		ChipBottomAnaly = 3
		Print "***ERROR ", 3
        Exit Function
		
	EndIf


	Double d_U
	'd_U = U_0 + 0.2
	'd_U = U_0 - tgt_u0
	d_U = tgt_u0 - U_0
	res(16) = d_U
	If Abs(d_U) < 2.0 Then
		Go Here -U(d_U)
	Else
		ChipBottomAnaly = 4
		Print "ERROR 4! : Rotation angle outside of control margin"
		Exit Function
	EndIf

	' Remeasure X and Y with correct rotation
	Wait 0.2
	'UF_take_picture(ByRef pict_fname$)
	pict_fname$ = UF_take_picture$(id$ + "_04")
	Print #fileNum, ",", pict_fname$,
	
	VRun ChipBottom_Analy
		
	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then
			
		VGet ChipBottom_Analy.Final.CameraX, X_0
		VGet ChipBottom_Analy.Final.CameraY, Y_0
		VGet ChipBottom_Analy.Final.Angle, U_0

		Print #fileNum, ",", ret_found,
		Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,

		res(17) = X_0
		res(18) = Y_0
		res(19) = U_0

	Else
		
		ChipBottomAnaly = 5
		Print "***ERROR ", 5
        Exit Function
			
	EndIf

	Double d_X, d_Y
	d_X = tgt_x0 - (X_0 - X_tool)
	d_Y = tgt_y0 - (Y_0 - Y_tool)
	
	Print #fileNum, ",", d_X, ",", d_Y, ",", d_U,

	res(20) = d_X
	res(21) = d_Y


	' Analysis of pins
	d_X = X_0 - X_tool
	d_Y = Y_0 - Y_tool
	If Abs(d_X) < 1 And Abs(d_Y) < 1 And Abs(U_0) < 2 Then
		Print "Positioning the chip for pin analysis: ",
		Print " dX=", d_X, " dY=", d_Y, " dU=", U_0
		Go Here -X(d_X) -Y(d_Y) +U(U_0)
	Else
		ChipBottomAnaly = 6
		Print "***ERROR ", 6
        Exit Function
	EndIf

	pict_fname$ = UF_take_picture$(id$ + "_pins")
	Print #fileNum, ",", pict_fname$,
	VSet pins_analy.ImageFile, pict_fname$
	
	Integer status
	status = PinsAnaly(id$)
	Print #fileNum, ",", status,
	If status <> 0 Then
		ChipBottomAnaly = status
	EndIf
		
	' Analysis of chip key for insertion into socket
	Boolean res_1, res_2, res_3, res_4
	If tgt_DAT_nr = 1 Then
		Print "Checking ASIC key"
		VSet key_check_1.ImageFile, pict_fname$
		VRun key_check_1
		
		VGet key_check_1.Blob01.Found, res_1
		VGet key_check_1.Blob02.Found, res_2
		VGet key_check_1.Blob03.Found, res_3
		VGet key_check_1.Blob04.Found, res_4
		
		If Not (res_1 And (Not res_2) And res_3 And res_4) Then
			Print "***ERROR! Failed to determine the key position of the ASIC"
			ChipBottomAnaly = 7
			Exit Function
		EndIf
	ElseIf tgt_DAT_nr = 2 Then
		Print "Checking ASIC key"
		VSet key_check.ImageFile, pict_fname$
		VRun key_check
		
		VGet key_check.Blob01.Found, res_1
		VGet key_check.Blob02.Found, res_2
		VGet key_check.Blob03.Found, res_3
		VGet key_check.Blob04.Found, res_4
		
		If Not (res_1 And res_2 And res_3 And (Not res_4)) Then
			Print "***ERROR! Failed to determine the key position of the ASIC"
			ChipBottomAnaly = 7
			Exit Function
		EndIf
	EndIf

	UF_camera_light_OFF
	
Fend

Function PinsRowAnaly(name$ As String, fileNum As Integer) As Integer
	
	PinsRowAnaly = 0
	
	Boolean passed
	Integer nFound, i
	Double x, y, area, xold, yold
		
	VGet pins_analy.name$.Passed, passed
	If Not passed Then
		Print "PinsAnaly " + name$ + " failed!"
		Print #fileNum, " failed"
		PinsRowAnaly = 301
		Exit Function
	EndIf

	VGet pins_analy.name$.NumberFound, nFound
	Print #fileNum, name$, ",", nFound,
	If nFound <> 32 Then
		PinsRowAnaly = 302
	EndIf

	For i = 1 To nFound
		VSet pins_analy.name$.CurrentResult, i
		VGet pins_analy.name$.CameraX, x
		VGet pins_analy.name$.CameraY, y
		VGet pins_analy.name$.Area, area
		Print #fileNum, ",", x, ",", y, ",", area,
		xold = x
		yold = y
		If i > 1 And Abs(x - xold) > 0.05 Then
			Print "*ERROR! Bent pin found in " + name$
			PinsRowAnaly = 400 + i
		EndIf
	Next i
	Print #fileNum, " "
	
Fend


Function PinsAnaly(id$ As String) As Integer
	
	PinsAnaly = 0
	
	Integer fileNum
	fileNum = FreeFile
	AOpen RTS_DATA$ + "\pins\" + id$ + "_pins.csv" As #fileNum
		
	
	VRun pins_analy
	
	Integer status
	status = PinsRowAnaly("BlobTop", fileNum)
	If status <> 0 Then
		PinsAnaly = status
	EndIf
	
	status = PinsRowAnaly("BlobBottom", fileNum)
	If status <> 0 Then
		PinsAnaly = status
	EndIf

	status = PinsRowAnaly("BlobLeft", fileNum)
	If status <> 0 Then
		PinsAnaly = status
	EndIf

	status = PinsRowAnaly("BlobRight", fileNum)
	If status <> 0 Then
		PinsAnaly = status
	EndIf

	Close #fileNum
	
Fend


' Prints error msg both to console and file
' closes the output file
Function RTS_error(fileNum AsInteger, err_msg$ As String)
	Print "***ERROR! ", err_msg$
	Print #fileNum, "***ERROR! ", err_msg$
	Close #fileNum
Fend


'
' INPUT: 
'        pallet_nr - pallet number of the chip source (1-left, 2-right)
'        col_nr - column number in the pallet (1-15)
'        row_nr - row number in the pallet (1-6)
'        DAT_nr - DAT board target (1-left, 2-right)
'        socket_nr - socket target (1-8)
'
' RETURN:
'        > 0 - job_id (timestamp)
'        < 0 - Error id


' Function MoveLARASICChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, larasic_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to LARASICs only
' 	 'larasic_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, larasic_socket_nr)
' Fend

' Function MoveCOLDADCChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldadc_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldadcs only
' 	 'coldadc_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, (coldadc_socket_nr+10))
' Fend

' Function MoveCOLDATAChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldata_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldatas only
' 	 'coldata_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, (coldata_socket_nr+20))
' Fend

' Function MoveChipFromTrayToChipSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, ChipType As String, socket_nr As Integer) As Int64
' 	 Inetger soc_nr
' 	 Select
' 		Case "LARASIC"
' 		     soc_nr=socket_nr
' 		Case "COLDADC"
' 		     soc_nr=socket_nr+10
' 		Case "COLDATA"
' 		     soc_nr=socket_nr+20
' 	Send
' 	MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
' Fend

Function MoveChipFromTrayToTypeSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, chip_type As Integer, socket_nr As Integer) As Int64
	 Integer soc_nr
	 soc_nr = socket_nr + 10 * chip_type
	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
Fend

Function MoveChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, socket_nr As Integer) As Int64
	
	String ts$
	ts$ = FmtStr$(Date$ + " " + Time$, "yyyymmddhhnnss")
	
	MoveChipFromTrayToSocket = Val(ts$)

	SetSpeed
	
	String fname$
	fname$ = "manip.csv"
	
	Integer fileNum
	fileNum = FreeFile
	AOpen RTS_DATA$ + "\" + fname$ As #fileNum
	
	Integer i
	Integer idx(20)
	For i = 1 To 20
		idx(i) = 0
	Next i
	
	idx(1) = fileNum
	
	Double res(30)
	

	idx(2) = pallet_nr
	idx(3) = col_nr
	idx(4) = row_nr
	idx(5) = 0
	idx(6) = 0
	idx(7) = 0
	idx(8) = 0
	idx(9) = 0
	idx(10) = DAT_nr
	idx(11) = socket_nr
			
	'String d$, t$
 	'd$ = Date$
	't$ = Time$
	'Print #fileNum, d$, " ", t$,
	Print #fileNum, ts$,
	' pallet source
	Print #fileNum, ",", pallet_nr, ",", col_nr, ",", row_nr,
	' pallet target
	Print #fileNum, ",", 0, ",", 0, ",", 0,
	' socket source
	Print #fileNum, ",", 0, ",", 0,
	' socket target
	Print #fileNum, ",", DAT_nr, ",", socket_nr,

	' Ensure that there is no chip in the socket
	If isChipInSocket(DAT_nr, socket_nr) Then
		RTS_error(fileNum, "Chip exists in the socket")
		Go Here :Z(-10)
        MoveChipFromTrayToSocket = -200
		Exit Function
	EndIf

	' Take a picture of the chip in the tray
	JumpToTray_camera(pallet_nr, col_nr, row_nr)
	String pict_fname$
	pict_fname$ = DF_take_picture$(ts$ + "_SN")
	Print #fileNum, ",", pict_fname$,

	'Print #fileNum, ",", chip_SN$,
	
	If Not isPressureOk Then
		RTS_error(fileNum, "Bad pressure")
        MoveChipFromTrayToSocket = -2
		Exit Function
	EndIf
				
	If Not isVacuumOk Then
		RTS_error(fileNum, "Bad vacuum")
        MoveChipFromTrayToSocket = -3
		Exit Function
	EndIf
		
				
	JumpToTray(pallet_nr, col_nr, row_nr)
	' Distort the pickup position on purpose
	'Go Here +Y(0.8)
	'Go Here -X(0.8)
	'Go Here -U(0.8)
	If Not PickupFromTray Then
		RTS_error(fileNum, "Can't pickup a chip from tray ")
        MoveChipFromTrayToSocket = -4
		Exit Function
	EndIf
		

	' Take picture of the bottom of the chip
	JumpToCamera
		
	Integer status
	'ChipBottomAnaly(chip_SN$, ByRef idx(), ByRef status, ByRef res())
	status = ChipBottomAnaly(ts$, ByRef idx(), ByRef res())

	If status <> 0 Then
		RTS_error(fileNum, "Analysis of chip bottom failed. Error = " + Str$(status))
        MoveChipFromTrayToSocket = -5
		Exit Function
	EndIf

	Double d_X, d_Y, d_U
	d_X = res(20)
	d_Y = res(21)
	d_U = res(16)
	
	' Analyzer pins	
	' Position the chip at the camera accordingly
	
	' Move to socket	
	JumpToSocket(DAT_nr, socket_nr)
	Wait 0.3
	' correct position
	Print "Correcting chip position for socket: ",
	Print " dX = ", d_X, " dY = ", d_Y, " dU = ", d_U
	If Abs(d_X) < 1.5 And Abs(d_Y) < 1.5 And Abs(d_U) < 2.0 Then
		Go Here +X(d_X) +Y(d_Y) -U(d_U)
		InsertIntoSocket
	Else
		RTS_error(fileNum, "Chip position out of limits")
        MoveChipFromTrayToSocket = -6
        Exit Function
	EndIf
	
	
	' Take picture of chip in the socket
	JumpToSocket_camera(DAT_nr, socket_nr)
	pict_fname$ = DF_take_picture$(ts$ + "_socket")
	Print #fileNum, ",", pict_fname$,
				
	Print #fileNum, " "
	
	Close #fileNum

Fend
	
'
' GLOBAL:

' Function MoveLARASICChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, larasic_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to LARASICs only
' 	 'larasic_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, larasic_socket_nr)
' Fend

' Function MoveCOLDADCChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldadc_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldadcs only
' 	 'coldadc_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, (coldadc_socket_nr+10))
' Fend

' Function MoveCOLDATAChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldata_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldatas only
' 	 'coldata_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, (coldata_socket_nr+20))
' Fend

' Function MoveChipFromChipSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, ChipType As String, socket_nr As Integer) As Int64
' 	 Inetger soc_nr
' 	 Select
' 		Case "LARASIC"
' 		     soc_nr=socket_nr
' 		Case "COLDADC"
' 		     soc_nr=socket_nr+10
' 		Case "COLDATA"
' 		     soc_nr=socket_nr+20
' 	Send
' 	MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
' Fend

Function MoveChipFromTypeSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, chip_type As Integer, socket_nr As Integer) As Int64
	 Integer soc_nr
	 soc_nr = socket_nr + 10 * chip_type
	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
Fend


Function MoveChipFromSocketToTray(DAT_nr As Integer, socket_nr As Integer, pallet_nr As Integer, col_nr As Integer, row_nr As Integer) As Int64
	
	String ts$
	ts$ = FmtStr$(Date$ + " " + Time$, "yyyymmddhhnnss")
	
	MoveChipFromSocketToTray = Val(ts$)

	SetSpeed
	
	String fname$
	fname$ = "manip.csv"
		
	Integer fileNum
	fileNum = FreeFile
	AOpen RTS_DATA$ + "\" + fname$ As #fileNum
		
	Integer i
	Integer idx(20)
	For i = 1 To 20
		idx(i) = 0
	Next i
	
	idx(1) = fileNum
	
	Double res(30)
		
	' new CSV entry - pickup from socket and return to tray
		
	idx(2) = 0
	idx(3) = 0
	idx(4) = 0
	idx(5) = pallet_nr
	idx(6) = col_nr
	idx(7) = row_nr
	idx(8) = DAT_nr
	idx(9) = socket_nr
	idx(10) = 0
	idx(11) = 0
	
	'String d$, t$
	'd$ = Date$
	't$ = Time$
	'Print #fileNum, d$, " ", t$,
	Print #fileNum, ts$,
	' pallet source
	Print #fileNum, ",", 0, ",", 0, ",", 0,
	' pallet target
	Print #fileNum, ",", pallet_nr, ",", col_nr, ",", row_nr,
	' socket source
	Print #fileNum, ",", DAT_nr, ",", socket_nr,
	' socket target
	Print #fileNum, ",", 0, ",", 0,

	' Ensure that there is no chip in destination	
	If isChipInTray(pallet_nr, col_nr, row_nr) Then
		RTS_error(fileNum, "Chip exists in the destination ")
		Go Here :Z(-10)
		MoveChipFromSocketToTray = -200
		Exit Function
	EndIf

	' Take picture of chip in the socket
	JumpToSocket_camera(DAT_nr, socket_nr)
	String pict_fname$
	pict_fname$ = DF_take_picture$(ts$ + "_socket")
	Print #fileNum, ",", pict_fname$,
	'DF_take_picture_socket(socket_nr, ByRef pict_fname_socket$)
	'Print #fileNum, ",", pict_fname_socket$,

	'Print #fileNum, ",", chip_SN$,
		
	If Not isPressureOk Then
		RTS_error(fileNum, "Bad pressure")
        MoveChipFromSocketToTray = -2
		Exit Function
	EndIf
				
	If Not isVacuumOk Then
		RTS_error(fileNum, "Bad vacuum")
        MoveChipFromSocketToTray = -3
		Exit Function
	EndIf
		
	' Pickup from socket
	JumpToSocket(DAT_nr, socket_nr)
	' Distort the pickup position on purpose
	'Go Here +Y(0.8)
	'Go Here -X(0.8)
	'Go Here +U(0.8)

	If Not PickupFromSocket Then
		RTS_error(fileNum, "Can't pickup chip from socket")
        MoveChipFromSocketToTray = -300
		Exit Function
	EndIf
			
	' Take picture of the bottom of the chip
	JumpToCamera
	
	Integer status
	'ChipBottomAnaly(chip_SN$, ByRef idx(), ByRef status, ByRef res())
	status = ChipBottomAnaly(ts$, ByRef idx(), ByRef res())

	If status <> 0 Then
		RTS_error(fileNum, "Analysis of chip bottom failed. Error = " + Str$(status))
        MoveChipFromSocketToTray = -4
		Exit Function
	EndIf

	Double d_X, d_Y, d_U
	d_X = res(20)
	d_Y = res(21)
	d_U = res(16)
	
	' Return to tray
	JumpToTray(pallet_nr, col_nr, row_nr)
	' correct position
	Print "Correcting chip position for tray: ",
	Print " dX = ", d_X, " dY = ", d_Y, " dU = ", d_U
		
	Wait 0.3
	If Abs(d_X) < 1.5 And Abs(d_Y) < 1.5 And Abs(d_U) < 2.0 Then
	'If Abs(d_X) < 3.0 And Abs(d_Y) < 3.0 And Abs(d_U) < 3.8 Then
		Go Here +X(d_X) +Y(d_Y) -U(d_U)
		If Not DropToTray Then
		'XN
			Go Here +Z(10) +X(0.1)
			Print "Try +X(0.1)"
			If Not DropToTray Then
				Go Here +Z(10) +Y(0.1)
				Print "Try +Y(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) -X(0.1)
					Print "Try -X(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) -X(0.1)
					Print "Try -X(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) -Y(0.1)
					Print "Try -Y(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) -Y(0.1)
					Print "Try -Y(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) +X(0.1)
					Print "Try +X(0.1)"
				If Not DropToTray Then
					Go Here +Z(10) +X(0.1)
					Print "Try +X(0.1)"
				If Not DropToTray Then
						RTS_error(fileNum, "Can't put chip into tray")
        				MoveChipFromSocketToTray = -6
        				Exit Function
        		EndIf
				EndIf
				EndIf
				EndIf
				
				EndIf

				EndIf
					
				EndIf
			
			EndIf
			
		'XN
			'RTS_error(fileNum, "Can't put chip into tray")
        	'MoveChipFromSocketToTray = -6
        	'Exit Function
		EndIf
	Else
		RTS_error(fileNum, "Chip position out of limits")
        MoveChipFromSocketToTray = -5
        Exit Function
	EndIf

	' Take a picture of the chip in the tray
	JumpToTray_camera(pallet_nr, col_nr, row_nr)
	pict_fname$ = DF_take_picture$(ts$ + "_SN")
	Print #fileNum, ",", pict_fname$,


	Print #fileNum, " "
	Close #fileNum

Fend


'
' INPUT: 
'        src_pallet_nr - pallet number of the chip source (1-left, 2-right)
'        src_col_nr - source column number (1-15)
'        src_row_nr - srouce row number (1-6)
'        tgt_pallet_nr - pallet number of chip destination (1-left, 2-right)
'        tgt_col_nr - column number in the destination (1-15)
'        tgt_row_nr - row number in the destination (1-6)
'        tgt_DU - chip rotation modification (deg.)
'
'
' RETURN:
'        > 0 - job_id (timestamp)
'        < 0 - Error id

Function MoveChipFromTrayToTray(src_pallet_nr As Integer, src_col_nr As Integer, src_row_nr As Integer, tgt_pallet_nr As Integer, tgt_col_nr As Integer, tgt_row_nr As Integer, tgt_DU As Double) As Int64
	
	String ts$
	ts$ = FmtStr$(Date$ + " " + Time$, "yyyymmddhhnnss")
	
	MoveChipFromTrayToTray = Val(ts$)

	SetSpeed
		
	String fname$
	fname$ = "manip.csv"
		
	Integer fileNum
	fileNum = FreeFile
	AOpen RTS_DATA$ + "\" + fname$ As #fileNum
		
	Integer i
	Integer idx(20)
	For i = 1 To 20
		idx(i) = 0
	Next i
	
	idx(1) = fileNum
	
	Double res(30)
	
			
	idx(2) = src_pallet_nr
	idx(3) = src_col_nr
	idx(4) = src_row_nr
	idx(5) = tgt_pallet_nr
	idx(6) = tgt_col_nr
	idx(7) = tgt_row_nr
	idx(8) = 0
	idx(9) = 0
	idx(10) = 0
	idx(11) = 0
			
	'String d$, t$
 	'd$ = Date$
	't$ = Time$
	'Print #fileNum, d$, " ", t$,
	Print #fileNum, ts$,
	' pallet source
	Print #fileNum, ",", src_pallet_nr, ",", src_col_nr, ",", src_row_nr,
	' pallet target
	Print #fileNum, ",", tgt_pallet_nr, ",", tgt_col_nr, ",", tgt_row_nr,
	' socket source
	Print #fileNum, ",", 0, ",", 0,
	' socket target
	Print #fileNum, ",", 0, ",", 0,

	' Ensure that there is no chip in destination	
	If src_pallet_nr <> tgt_pallet_nr Or src_col_nr <> tgt_col_nr Or src_row_nr <> tgt_row_nr Then
		If isChipInTray(tgt_pallet_nr, tgt_col_nr, tgt_row_nr) Then
			RTS_error(fileNum, "Chip exists in the destination ")
			Go Here :Z(-10)
			MoveChipFromTrayToTray = -200
			Exit Function
		EndIf
	EndIf


	' Take a picture of the chip in the tray
	JumpToTray_camera(src_pallet_nr, src_col_nr, src_row_nr)
	String pict_fname$
	pict_fname$ = DF_take_picture$(ts$ + "_SN")
	Print #fileNum, ",", pict_fname$,

	'Print #fileNum, ",", chip_SN$,
		
	' Ensure that chip exists in the source	
	'If Not isChipInTray(src_pallet_nr, src_col_nr, src_row_nr) Then
	'	RTS_error(fileNum, "No chip found in the tray ")
	'	MoveChipFromTrayToTray = -201
	'	Exit Function
	'EndIf
		
	If Not isVacuumOk Then
		RTS_error(fileNum, "Bad vacuum ")
		MoveChipFromTrayToTray = -202
		Exit Function
	EndIf
	
	JumpToTray(src_pallet_nr, src_col_nr, src_row_nr)

	' If chip rotation requested:
	If tgt_DU <> 0.0 Then
		Double Utmp
		Utmp = CU(Pallet(src_pallet_nr, src_col_nr, src_row_nr)) - tgt_DU
		If Utmp > 360 Then
			Utmp = Utmp - 360
		ElseIf Utmp < 0 Then
			Utmp = Utmp + 360
		EndIf
		If Utmp < 0 Or Utmp > 360 Then
			RTS_error(fileNum, "Internal program error - bad tgt_DU ")
			MoveChipFromTrayToTray = -4
			Exit Function
		EndIf
		Go Here :U(Utmp)
	EndIf

	' Distort the pickup position on purpose
	'Go Here -Y(0.8)
	'Go Here +X(0.8)
	'Go Here -U(0.8)
	If Not PickupFromTray Then
		RTS_error(fileNum, "Can't pickup a chip from tray ")
        MoveChipFromTrayToTray = -5
		Exit Function
	EndIf
	
	' Take picture of the bottom of the chip
	JumpToCamera
	
	Integer status
	'ChipBottomAnaly(chip_SN$, ByRef idx(), ByRef status, ByRef res())
	status = ChipBottomAnaly(ts$, ByRef idx(), ByRef res())

	If status <> 0 Then
		RTS_error(fileNum, "Analysis of chip bottom failed. Error = " + Str$(status))
        MoveChipFromTrayToTray = -6
		Exit Function
	EndIf

	Double d_X, d_Y, d_U
	d_X = res(20)
	d_Y = res(21)
	d_U = res(16)
	
	' Move to tray
	JumpToTray(tgt_pallet_nr, tgt_col_nr, tgt_row_nr)
	Wait 0.2
	If Not (src_pallet_nr = tgt_pallet_nr And src_row_nr = tgt_row_nr And src_col_nr = tgt_col_nr And tgt_DU = 0) Then
		' correct position
		Print "Correcting chip position for tray: ",
		Print " dX = ", d_X, " dY = ", d_Y, " dU = ", d_U
		If Abs(d_X) < 1.5 And Abs(d_Y) < 1.5 And Abs(d_U) < 2.0 Then
			Go Here +X(d_X) +Y(d_Y) -U(d_U)
		Else
			RTS_error(fileNum, "Chip position out of limits")
        	MoveChipFromTrayToTray = -7
        	Exit Function
		EndIf
	Else
		Print "Chip position correction for tray: ",
		Print " dX = ", d_X, " dY = ", d_Y, " dU = ", d_U
		Print "No correction of chip position applied because orgin and destination are the same"
	EndIf
	If Not DropToTray Then
		RTS_error(fileNum, "Can't put chip into tray")
       	MoveChipFromTrayToTray = -8
       	Exit Function
	EndIf

	
	
	' Take a picture of the chip in the tray
	JumpToTray_camera(tgt_pallet_nr, tgt_col_nr, tgt_row_nr)
	pict_fname$ = DF_take_picture$(ts$ + "_SN")
	Print #fileNum, ",", pict_fname$,

				
	Print #fileNum, " "
	Close #fileNum

Fend
	
Function calibrate_socket(DAT_nr As Integer, socket_nr As Integer)
	Print
	Print socket_nr, "**********************************************"
	
	
	JumpToSocket_camera(DAT_nr, socket_nr)
	
	'Add error: x:  0.888672y:  -0.798645
	'Go Here +X(0.888672)
	'Go Here +Y(-0.798645)
	
	'Add a position fluctuation for test, only for test!!!	
	
	'Real r_x
  	'Randomize
  	'r_x = Rnd(2) - 1
  	
  	'Real r_y
    'Randomize
    'r_y = Rnd(2) - 1
  	
  	'Go Here +X(r_x)
  	'Go Here +Y(r_x)
  	'Print "Add error: x: ", r_x, "y: ", r_y
  	
  	'random end ********************************************************

	
	VRun skt_cali_test
	'Integer nP
	'VGet skt_cali_test.Geom01.NumberFound, nP
	'Print "number of point found: ", nP
	
	Boolean Isfound1, Isfound2, Isfound3
	Boolean found
	'VGet skt_cali_test.Geom01.Found, Isfound1
	'VGet skt_cali_test.Geom02.Found, Isfound2
	'VGet skt_cali_test.Geom03.Found, Isfound3
	
	Double x_p1, y_p1, a_p1, x_p2, y_p2, a_p2, x_p3, y_p3, a_p3
	Double x_ori, y_ori, a_ori
	
	'VGet skt_cali_test.CameraCenter.RobotXYU, found, x_ori, y_ori, a_ori
	Double check
	check = 100
	Integer N_round
	N_round = 0
	
	Do Until check < 20 And check > -20 Or N_round > 10
		VRun skt_cali_test
		VGet skt_cali_test.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		'Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		'Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		'Print "P3 xyu: ", x_p3, y_p3, a_p3
	

		check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
		N_round = N_round + 1
		'Print "perpendicular check: ", check, " Loop: ", N_round
	
	Loop
	
	
	If check < 20 And check > -20 Then
		Print "Correctly found"
	EndIf
	
	
	Double x_c, y_c
	
	x_c = (x_p1 + x_p3) /2
	y_c = (y_p1 + y_p3) /2
	'Print "HERE: ", Here
	'Print "ori_center: ", x_ori, y_ori
	Print "corr_center: ", x_c, y_c
	'Print P(20 + socket_nr) :Z(-132.5)
	
	'Double A_line
	
	'VGet skt_cali_test.LineFind01.Angle, A_line
	'Print A_line
	
	
Fend

Function XOffset(UValue As Double) As Double
	XOffset = DF_CAM_X_OFF_U0 * Cos(DegToRad(UValue - HAND_U0)) - DF_CAM_Y_OFF_U0 * Sin(DegToRad(UValue - HAND_U0))
Fend

Function YOffset(UValue As Double) As Double
	YOffset = DF_CAM_X_OFF_U0 * Sin(DegToRad(UValue - HAND_U0)) + DF_CAM_Y_OFF_U0 * Cos(DegToRad(UValue - HAND_U0))
Fend

Function FindChipDirectionWithDF As Double '(UseCoordinates As Boolean) As Double
	
	Double m, UChip
	
	Print "EOAT is at (", CX(Here), ",", CY(Here), ",Z,", CU(Here), ")"
	
	Boolean isFoundL, isFoundS
	
	VRun GetChipDir
	
	' Get positions of Large and Small circular markers on chip
	Double xL, yL, uL, xS, yS, uS
	
'	If UseCoordinates Then
		Print "Using robot coordinates, will need DF cam calibration"
		VGet GetChipDir.Geom01.RobotXYU, isFoundL, xL, yL, uL
		VGet GetChipDir.Geom02.RobotXYU, isFoundS, xS, yS, uS
'	Else
'		VGet GetChipDir.Geom01.PixelXYU, isFoundL, xL, yL, uL
'		VGet GetChipDir.Geom02.PixelXYU, isFoundS, xS, yS, uS
'	EndIf

	If (Not isFoundL) Or (Not isFoundS) Then
		Print "ERROR: Cannot find chip fiducial marks"
		FindChipDirectionWithDF = -999.
		Exit Function
	EndIf
	
	Print "Large fiducial marker found at: x=", xL, "; y=", yL ', "; u=", uL
	Print "Small fiducial marker found at: x=", xS, "; y=", yS ', "; u=", uS
	
	Double AvX, AvY
	AvX = (xL + xS) /2
	AvY = (yL + yS) /2
	Print "Average X and Y: ( ", AvX, ",", AvY, " )"
	
	
	Double DelX, DelY, Hyp, SPolar
	DelX = xS - xL
	DelY = yS - yL
	Hyp = Sqr((DelX * DelX) + (DelY * DelY))
	SPolar = RadToDeg(Acos(DelY / Hyp))
	Print "DelX = xL - xS = ", DelX
	Print "DelX = yL - yS = ", DelY
	Print "Hypotenuse = ", Hyp
	Print "Polar angle of small mark from large marg ", SPolar
		
		
'		m = DelY / DelX
'		Print "DelY/DelX = ", m
'		
'		' Remember pixels are upside down	
'		
'	EndIf
	FindChipDirectionWithDF = SPolar - 45.
	
Fend

