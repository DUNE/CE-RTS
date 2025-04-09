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
	If RotateFirst Then
		Go Here +U(dU)
		Move Here -X(XOffset(CU(Here) - dU)) -Y(YOffset(CU(Here) - dU))
	Else
		Move Here -X(XOffset(CU(Here))) -Y(YOffset(CU(Here)))
		Go Here +U(dU)
	EndIf
	Move Here -Z(DF_CAM_Z_OFF)
	
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
	
	If ChipType$ = "COLDATA" Then
		If pallet_nr = 1 Then
			Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0)) +Y(YOffset(HAND_U0)) +Z(DF_CAM_Z_OFF) :U(HAND_U0)
		ElseIf pallet_nr = 2 Then
			Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0)) +Y(YOffset(HAND_U0)) +Z(DF_CAM_Z_OFF) :U(HAND_U0)
		EndIf

	Else
		If pallet_nr = 1 Then
			Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0)) +Y(YOffset(HAND_U0)) +Z(DF_CAM_Z_OFF) :U(HAND_U0)
		ElseIf pallet_nr = 2 Then
			Jump Pallet(pallet_nr, col_nr, row_nr) +X(XOffset(HAND_U0 + 180)) +Y(YOffset(HAND_U0 + 180)) +Z(DF_CAM_Z_OFF) :U(HAND_U0 + 180)
		EndIf
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
	' Assumes that the starting position is 10mm above the chip tray,
	' Moves down 5mm and then drops the chip into the tray. Returns
	' false if the contact sensor touches something, otherwise true.
	
	DropToTray = False
	Speed 1
	Accel 1, 1
    Go Here -Z(5) Till Sw(8) = On Or Sw(9) = Off
	If isContactSensorTouches Then
		Print "ERROR! Contact Sensor detects obstacle in the tray"
		Exit Function
	EndIf
	
	VacuumValveClose
    Wait 2
    Go Here +Z(5)
    SetSpeed
    DropToTray = True
Fend


Function JumpToSocket(DAT_nr As Integer, socket_nr As Integer)
	If DAT_nr = 2 Then
        'Jump P(200 + socket_nr) :Z(-132.5)
        Jump P(200 + socket_nr) +Z(10) ' Go 10mm above the socket position to avoid collisions
		Print P(200 + socket_nr)
	ElseIf DAT_nr = 1 Then
		'Jump P(100 + socket_nr) -X(DF_CAMERA_OFFSET) :Z(-134.682)
		Jump P(100 + socket_nr) +Z(10)
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
	Move Here -Z(10) ' To correct for offset TC added to JumpToSocket
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

Function DropToSocket As Boolean
	' Assumes you are +10 above the socket defined position and the plunger
	' can fully open the socket by moving down at most 14mm. This function
	' checks the pressure, opens the plunger, opens the socket, drops the
	' chip, then moves back up 10mm.
	
	DropToSocket = False
	
	' Check the pressure is ok
	If Not isPressureOk Then
		Exit Function
	EndIf
	
	' Turn the pluger on to open the socket
	Wait 1
	PlungerOn
    Wait 1
	
	Go Here -Z(14) Till Sw(8) = On Or Sw(9) = Off
	
	' Close the valve to drop the chip
	Wait 1
	VacuumValveClose
	Wait 1
	
    ' Go back up 
    Go Here +Z(10)
    
	' Turn the pluger off
	Wait 1
	PlungerOff
    Wait 1
	
Fend



Function PickupFromSocket As Boolean
	' This function assumes the stinger is 10mm above the socket position. It
	' checks the vacuum and pressure, opens the plunger, moves down until 
	' contact is made with the chip, turns the vacuum on, and goes up with
	' the chip. 

	PickupFromSocket = False

	If Not isVacuumOk Then
		Exit Function
	EndIf

	If Not isPressureOk Then
		Exit Function
	EndIf
	
	PlungerOn
	Wait 1
	Go Here -Z(10)


	Speed 1
	Accel 1, 1

	Boolean TouchSuccess ' Can't just directly use Not Byte for converting 0 to success
	TouchSuccess = Not TouchChip ' Should be 0 for touch, non zero error code
	If Not TouchSuccess Then
		Print "ERROR! Cannot pick up from socket"
		Exit Function
	EndIf

	Wait 1
	VacuumValveOpen

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
	' Jump to a given DAT_nr and socket_nr with the DF camera focusing
	' on the socket. Do not add an additional rotoation if using COLDATA.
	
	If ChipType$ = "COLDATA" Then
		If DAT_nr = 2 Then
			Jump P(200 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here))) +Y(YOffset(CU(Here)))
		ElseIf DAT_nr = 1 Then
			Jump P(100 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here))) +Y(YOffset(CU(Here)))
		EndIf

	Else
		If DAT_nr = 2 Then
			Jump P(200 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here) - 45)) +Y(YOffset(CU(Here) - 45)) -U(45)
		ElseIf DAT_nr = 1 Then
			Jump P(100 + socket_nr) +Z(DF_CAM_Z_OFF) +X(XOffset(CU(Here) + 135)) +Y(YOffset(CU(Here) + 135)) +U(135)
		EndIf
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


' Function MoveLArASICChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, larasic_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to LArASICs only
' 	 'larasic_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, larasic_socket_nr)
' Fend

' Function MoveColdADCChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldadc_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldadcs only
' 	 'coldadc_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, (coldadc_socket_nr+10))
' Fend

' Function MoveColDATAChipFromTrayToSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldata_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldatas only
' 	 'coldata_socket_nr
' 	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, (coldata_socket_nr+20))
' Fend

' Function MoveChipFromTrayToChipSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, ChipType As String, socket_nr As Integer) As Int64
' 	 Inetger soc_nr
' 	 Select
' 		Case "LArASIC"
' 		     soc_nr=socket_nr
' 		Case "ColdADC"
' 		     soc_nr=socket_nr+10
' 		Case "ColDATA"
' 		     soc_nr=socket_nr+20
' 	Send
' 	MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
' Fend

Function MoveChipFromTrayToTypeSocket(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, chip_type As Integer, socket_nr As Integer) As Int64
	 Integer soc_nr
	 soc_nr = socket_nr + 10 * chip_type
	 MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, soc_nr)
Fend

Function MoveChipFromTrayToSocket(DAT_nr As Integer, socket_nr As Integer, pallet_nr As Integer, col_nr As Integer, row_nr As Integer) As Int64
	
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
	'If isChipInSocket(DAT_nr, socket_nr) Then
	'	RTS_error(fileNum, "Chip exists in the socket")
	'	Go Here :Z(-10)
    '    MoveChipFromTrayToSocket = -200
	'	Exit Function
	'EndIf

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

	If Not PickupFromTray Then
		RTS_error(fileNum, "Can't pickup a chip from tray ")
        MoveChipFromTrayToSocket = -4
		Exit Function
	EndIf
		

	' Take picture of the bottom of the chip
	Jump P_camera 'JumpToCamera
		
	Integer status
	Double corrs(2)
	If ChipType$ = "COLDATA" Then
		status = COLDATA_VisAnalysis(ByRef corrs())
	Else
		status = ChipBottomAnaly(ts$, ByRef idx(), ByRef res())
	EndIf

	If status <> 0 Then
		RTS_error(fileNum, "Analysis of chip bottom failed. Error = " + Str$(status))
        MoveChipFromTrayToSocket = -5
		Exit Function
	EndIf
	
	' Move to socket	
	JumpToSocket(DAT_nr, socket_nr)
	Wait 0.3
	

	Double X_CORR_tray, Y_CORR_tray, U_CORR_tray
	If ChipType$ = "COLDATA" Then
		X_CORR_tray = corrs(1)
		Y_CORR_tray = corrs(2)
		U_CORR_tray = 0
	Else
		X_CORR_tray = res(20)
		Y_CORR_tray = res(21)
		U_CORR_tray = res(16)
	EndIf
	
	Print "Correcting chip position from tray: ",
	Print " dX = ", X_CORR_tray, " dY = ", Y_CORR_tray, " dU = ", U_CORR_tray
	
	' Save/update the socket position corrections
	tray_X(pallet_nr, col_nr, row_nr) = X_CORR_tray
	tray_Y(pallet_nr, col_nr, row_nr) = Y_CORR_tray
	tray_U(pallet_nr, col_nr, row_nr) = U_CORR_tray
	
	' Grab the socket position corrections 
	Double X_CORR_socket, Y_CORR_socket, U_CORR_socket
	X_CORR_socket = DAT_X(DAT_nr, socket_nr)
	Y_CORR_socket = DAT_Y(DAT_nr, socket_nr)
	U_CORR_socket = 0
	Print "Saved socket corrections:", X_CORR_socket, Y_CORR_socket
	
	' Note: Corrections for socket are in the same frame
	Double d_X, d_Y, d_U
	d_X = X_CORR_tray - X_CORR_socket
	d_Y = Y_CORR_tray - Y_CORR_socket
	d_U = U_CORR_tray - U_CORR_socket
	
	' correct position
	Print "Correcting chip position for socket: ",
	Print " dX = ", d_X, " dY = ", d_Y, " dU = ", d_U
	If Abs(d_X) < 15 And Abs(d_Y) < 15 And Abs(d_U) < 20 Then
		Go Here -X(d_X) -Y(d_Y) -U(d_U)
		DropToSocket
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

' Function MoveLArASICChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, larasic_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to LArASICs only
' 	 'larasic_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, larasic_socket_nr)
' Fend

' Function MoveColdADCChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldadc_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldadcs only
' 	 'coldadc_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, (coldadc_socket_nr+10))
' Fend

' Function MoveColDATAChipFromSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, coldata_socket_nr As Integer) As Int64
' 	 'Wraps generic function and restricts to Coldatas only
' 	 'coldata_socket_nr
' 	 MoveChipFromSocketToTray(pallet_nr, col_nr, row_nr, DAT_nr, (coldata_socket_nr+20))
' Fend

' Function MoveChipFromChipSocketToTray(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, ChipType As String, socket_nr As Integer) As Int64
' 	 Inetger soc_nr
' 	 Select
' 		Case "LArASIC"
' 		     soc_nr=socket_nr
' 		Case "ColdADC"
' 		     soc_nr=socket_nr+10
' 		Case "ColDATA"
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
	'If isChipInTray(pallet_nr, col_nr, row_nr) Then
	'	RTS_error(fileNum, "Chip exists in the destination ")
	'	Go Here :Z(-10)
	'	MoveChipFromSocketToTray = -200
	'	Exit Function
	'EndIf

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
	Jump P_camera 'JumpToCamera
	
	Integer status
	Double corrs(2)
	If ChipType$ = "COLDATA" Then
		status = COLDATA_VisAnalysis(ByRef corrs())
	Else
		'ChipBottomAnaly(chip_SN$, ByRef idx(), ByRef status, ByRef res())
		status = ChipBottomAnaly(ts$, ByRef idx(), ByRef res())
	EndIf

	If status <> 0 Then
		RTS_error(fileNum, "Analysis of chip bottom failed. Error = " + Str$(status))
        MoveChipFromSocketToTray = -4
		Exit Function
	EndIf

	Double X_CORR_socket, Y_CORR_socket, U_CORR_socket
	If ChipType$ = "COLDATA" Then
		X_CORR_socket = corrs(1)
		Y_CORR_socket = corrs(2)
		U_CORR_socket = 0
	Else
		X_CORR_socket = res(20)
		Y_CORR_socket = res(21)
		U_CORR_socket = res(16)
	EndIf
	
	Print "Correcting chip position from socket: ",
	Print " dX = ", X_CORR_socket, " dY = ", Y_CORR_socket, " dU = ", U_CORR_socket
	
	' Save/update the socket position corrections
	DAT_X(DAT_nr, socket_nr) = X_CORR_socket
	DAT_Y(DAT_nr, socket_nr) = Y_CORR_socket
	DAT_U(DAT_nr, socket_nr) = U_CORR_socket
	
	' Jump to the given tray
	JumpToTray(pallet_nr, col_nr, row_nr)
	
	' Correct for socket offset (socket corr is rotation -90 degrees)
	'Move Here -X(-Y_CORR_socket) -Y(X_CORR_socket)
	
	' Rotate to match chip placement
    'Go Here -U(90) ' NOTE: temporary correction added for FNAL due to chip orientation
		
	Wait 0.3
	
	' Grab the tray position corrections
	Double X_CORR_tray, Y_CORR_tray, U_CORR_tray
	X_CORR_tray = tray_X(pallet_nr, col_nr, row_nr)
	Y_CORR_tray = tray_Y(pallet_nr, col_nr, row_nr)
	U_CORR_tray = 0 'tray_U(pallet_nr, col_nr, row_nr)
	Print "Saved tray corrections:", X_CORR_tray, Y_CORR_tray
	
	' Correct for tray offset
	'Move Here +X(-Y_CORR_tray) +Y(X_CORR_tray)
	
	Double d_X, d_Y, d_U
	d_X = -Y_CORR_socket + Y_CORR_tray
	d_Y = X_CORR_socket - X_CORR_tray
	d_U = U_CORR_socket - U_CORR_tray
	
	' Only correct if corrections are under some limit
	If Abs(d_X) < 50.0 And Abs(d_Y) < 50.0 And Abs(d_U) < 180 Then
	
		' Make both tray and socket corrections
		Move Here -X(d_X) -Y(d_Y)

		If Not DropToTray Then
			RTS_error(fileNum, "Failed to DropToTray")
			MoveChipFromSocketToTray = -6
       		Exit Function
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
	Jump P_camera 'JumpToCamera
	
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

Function SetupDirectories
	' Creates the directory RTS_DATA set in RTS_tools.inc if not already made 
	' And the subdirectories \images and \pins  
	
	' RTS DATA directory (set in RTS_tools.inc)
	If Not FolderExists(RTS_DATA$) Then
  		MkDir RTS_DATA$
	EndIf

	If Not FolderExists(RTS_DATA$) Then
  		Print "***ERROR Can't create directory [" + RTS_DATA$ + "]"
  		Exit Function
	EndIf
	
	' images subdirectory
	String dir_images$
	dir_images$ = RTS_DATA$ + "images"

	If Not FolderExists(dir_images$) Then
  		MkDir dir_images$
	EndIf
	
	If Not FolderExists(dir_images$) Then
  		Print "***ERROR Can't create directory [" + dir_images$ + "]"
  		Exit Function
	EndIf
	
	' pins subdirectory
	String dir_pins$
	dir_pins$ = RTS_DATA$ + "pins"

	If Not FolderExists(dir_pins$) Then
  		MkDir dir_pins$
	EndIf
	
	If Not FolderExists(dir_pins$) Then
  		Print "***ERROR Can't create directory [" + dir_pins$ + "]"
  		Exit Function
	EndIf
Fend

Function MakePositionFiles
	' Sets the global arrays tray_X, tray_Y, tray_U, DAT_X, DAT_Y, and DAT_U to all zeros	
	' Then updates/makes the files tray_xyu.csv and .csv to the new array values.	
	' The arrays represent offsets of where the EOAT comes in contact and the center of the
	' tray slot or socket
	
	' reset global arrays for tray corrections
	Integer i, j, k
	For i = 1 To NTRAYS
		For j = 1 To TRAY_NCOLS
			For k = 1 To TRAY_NROWS
				tray_X(i, j, k) = 0
				tray_Y(i, j, k) = 0
				tray_U(i, j, k) = 0
			Next k
		Next j
	Next i
	
	' reset global arrays for socket positions
	For i = 1 To 2
		For j = 1 To NSOCKETS
			DAT_X(i, j) = 0
			DAT_Y(i, j) = 0
			DAT_U(i, j) = 0
		Next j
	Next i
	
	' save a file to keep track of tray corrections
	Integer fileNum
	String fileName$
	fileNum = FreeFile ' Returns an unused file handle
	fileName$ = RTS_DATA$ + "\tray_xyu.csv"
	WOpen fileName$ As #fileNum
	For i = 1 To NTRAYS
		For j = 1 To TRAY_NCOLS
			For k = 1 To TRAY_NROWS
				Print #fileNum, i, ",", j, ",", k, ",", tray_X(i, j, k), ",", tray_Y(i, j, k), ",", tray_U(i, j, k)
			Next k
		Next j
	Next i
	Close #fileNum
    
	' save a file to keep track of socket corrections
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\socket_xyu.csv"
	WOpen fileName$ As #fileNum
	For i = 1 To 2
		Print "NSOCKETS", NSOCKETS
		For j = 1 To NSOCKETS
			Print #fileNum, i, ",", j, ",", DAT_X(i, j), ",", DAT_Y(i, j), ",", DAT_U(i, j)
		Next j
	Next i
	Close #fileNum
	
Fend

Function UpdatePositionFiles
	'Updates the files holding the position corrections for the trays (tray_xyu.csv) 
	' and the sockets on the DAT board (socket_xyu.csv)
	
	' Set the file name for the tray position corrections
	Integer fileNum
	String fileName$
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\tray_xyu.csv"
	WOpen fileName$ As #fileNum
	
	' Save the position set in the global arrays to the files
	Integer i, j, k
	For i = 1 To NTRAYS
		For j = 1 To TRAY_NCOLS
			For k = 1 To TRAY_NROWS
				Print #fileNum, i, ",", j, ",", k, ",", tray_X(i, j, k), ",", tray_Y(i, j, k), ",", tray_U(i, j, k)
			Next k
		Next j
	Next i
	Close #fileNum
    
	' Set the file name for the socket position corrections
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\socket_xyu.csv"
	WOpen fileName$ As #fileNum
	
	' Save the position set in the global arrays to the files
	For i = 1 To 2
		For j = 1 To NSOCKETS
			Print #fileNum, i, ",", j, ",", DAT_X(i, j), ",", DAT_Y(i, j), ",", DAT_U(i, j)
		Next j
	Next i
	Close #fileNum
Fend

Function LoadPositionFiles
	' load positions at camera of chips coming from trays
	
    Integer fileNum
    String fileName$
    Double x, y, u
    Double i, j, k
    Double ii, jj, kk
    
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\tray_xyu.csv"
	
	If FileExists(fileName$) Then
		Print "Reading file ", fileName$
		ROpen fileName$ As #fileNum
		For i = 1 To NTRAYS
			For j = 1 To TRAY_NCOLS
				For k = 1 To TRAY_NROWS
					Input #fileNum, ii, jj, kk, x, y, u
					If ii = i And jj = j And kk = k Then
						tray_X(i, j, k) = x
						tray_Y(i, j, k) = y
						tray_U(i, j, k) = u
					Else
						Print "Error reading file ", fileName$, " ijk=", i, " ", j, " ", k
						Exit Function
					EndIf
				Next k
			Next j
		Next i
	EndIf
	Close #fileNum
	
	' load positions at camera of chips coming from sockets
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\socket_xyu.csv"
	If FileExists(fileName$) Then
		Print "Reading file ", fileName$
		ROpen fileName$ As #fileNum
		For i = 1 To 2
			For j = 1 To NSOCKETS
				Input #fileNum, ii, jj, x, y, u
				If ii = i And jj = j Then
					DAT_X(i, j) = x
					DAT_Y(i, j) = y
					DAT_U(i, j) = u
				Else
					Print "Error reading file ", fileName$
					Exit Function
				EndIf
			Next j
		Next i
	EndIf
	Close #fileNum
Fend


Function COLDATA_VisAnalysis(ByRef corrections() As Double) As Integer
	' Uses the upward facing camera to find the offset of the EOAT center of rotation
	' and the chip center. Assumes the EOAT is currently holding a chip, it uses COLDATA_Corrs
	' to find the chip center, then rotates it 180 degrees at runs it again. The difference
	' in the center positions results in the offset of the EOAT center and chip center.
	
	Print "COLDATA_VisAnalysis start: ", Here
	
	' Turn the EOAT light on for better pictures of chip	
	On 12
	
	' Run the vision sequence to find the chip center
	VRun COLDATA_Corrs
	
	' Save the initial center position of the chip
	Double X_0, Y_0, U_0
	VGet COLDATA_Corrs.FindChip.CameraX, X_0
	VGet COLDATA_Corrs.FindChip.CameraY, Y_0
	VGet COLDATA_Corrs.FindChip.Angle, U_0
	
	Print "X, Y, U: ", X_0, Y_0, U_0
	
	' Rotate 180 degrees
	Go Here +U(180)
	
	' Fine the new center of the chip after rotation
	VRun COLDATA_Corrs
	
	' Save the new center position of the chip after rotation
	Double X_180, Y_180, U_180
	VGet COLDATA_Corrs.FindChip.CameraX, X_180
	VGet COLDATA_Corrs.FindChip.CameraY, Y_180
	VGet COLDATA_Corrs.FindChip.Angle, U_180
	
	Print "X2, Y2, U2: ", X_180, Y_180, U_180
	
	' Calculate the center of the tool (EAOT)
	Double X_tool, Y_tool
	X_tool = 0.5 * (X_0 + X_180)
	Y_tool = 0.5 * (Y_0 + Y_180)
	
	' Calculate the correction to center the chip
	Double X_COR, Y_COR
	X_COR = 0.5 * (X_0 - X_180)
	Y_COR = 0.5 * (Y_0 - Y_180)
	Print "X_COR, YCOR: ", X_COR, Y_COR
	
	' Rotate back to initial position
	Go Here -U(180)
	
	' Turn the light off
	Off 12
	
	' Save corrections
	corrections(1) = X_COR
	corrections(2) = Y_COR
	'corrections(3) = 0 'U_COR
	
Fend

Function GetTrayCorrection(pallet_nr As Integer, col_nr As Integer, row_nr As Integer)
	' This functions picks up a chip, runs the vision sequence to get the corrections	
	' needed, so that a new tray/site can collect the initial chip correction information.
	' This function would need to be used on each chip position before using the functions
	' MoveChipFromTrayToSocket or MoveChipFromSocketToTray
	
	' Jump to the given tray position
	JumpToTray(pallet_nr, col_nr, row_nr)
	
	' Attempt to pickup chip from tray, exit function if it fails
	If Not PickupFromTray Then
		Print "Can't pickup chip from socket"
		Exit Function
	EndIf
	
	' Move to the upward facing camera for corrections
	Jump P_Camera
	Wait 1
	
	' Run the calibration sequence and save the correction results
	Double corrs(2)
	Double res(3)
	Integer status
	
	If CHIPTYPE$ = "COLDATA" Then
		status = COLDATA_VisAnalysis(ByRef corrs())
	Else
		Print "ERROR: Not implemented for other chip types yet"
		Exit Function
	EndIf
		
	
	Print "Previous tray corr:", tray_X(pallet_nr, col_nr, row_nr), tray_Y(pallet_nr, col_nr, row_nr)
	Print "New corrections:", corrs(1), corrs(2)
	
	' Grab corrections from camera calibration
	Double X_CORR_tray, Y_CORR_tray
	X_CORR_tray = corrs(1)
	Y_CORR_tray = corrs(2)
	
	' Save/update the tray position corrections
	tray_X(pallet_nr, col_nr, row_nr) = X_CORR_tray
	tray_Y(pallet_nr, col_nr, row_nr) = Y_CORR_tray
	
	' Jump to the given tray position
	JumpToTray(pallet_nr, col_nr, row_nr)
	
	DropToTray
	
Fend

Function GetAllTrayCorrections(pallet_nr As Integer)
	' This function runs GetTrayCorrection for all chips in a tray,
	' in order to fill the tray_xyu.csv files initially. This should
	' only be needed the first time and whenever the tray_xyu.csv
	' file is reset. 
	
	Integer i, j
	For i = 1 To TRAY_NCOLS
		For j = 1 To TRAY_NROWS
			GetTrayCorrection(pallet_nr, i, j)
		Next j
	Next i
	
Fend

