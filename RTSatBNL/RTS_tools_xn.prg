#include "RTS_tools.inc"

Function calibrate_socket(DAT_nr As Integer, socket_nr As Integer)
	'Print socket_nr, "**********************************************"
	'this function is only used for test and debug
	
	'Double uerr
  	'Randomize
	'uerr = Rnd(3) - 1.5
	'Print uerr
	JumpToSocket_camera(DAT_nr, socket_nr)
	'Go Here +U(uerr)
	'Go Here +Y(DF_CAMERA_OFFSET / 2)
	'Go Here -X(DF_CAMERA_OFFSET - DF_CAMERA_OFFSET / 2 * 1.73)
	
	'Add error: x:  0.888672y:  -0.798645
	'Go Here +X(0.888672)
	'Go Here +Y(-0.798645)
	
	'Add a position fluctuation for test, only for test!!!	
	
	'Real r_x
  	'Randomize
  	'r_x = Rnd(3) - 1.5
  	
  	'Real r_y
    'Randomize
    'r_y = Rnd(3) - 1.5
  	
  	'Go Here +X(r_x)
  	'Go Here +Y(r_x)
  	'Print "Add error: x: ", r_x, "y: ", r_y
  	
  	'random end ********************************************************


	
	'VRun skt_cali_test
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
	Double x_p1_cam, y_p1_cam, x_p2_cam, y_p2_cam, x_p3_cam, y_p3_cam
	
	'VGet skt_cali_test.CameraCenter.RobotXYU, found, x_ori, y_ori, a_ori
	Double check
	Integer N_round
	If DAT_nr = 2 Then
	
		
		check = 100
		
		N_round = 0
		Do Until check < 20 And check > -20 Or N_round > 10
		VRun skt_cali_test
		VGet skt_cali_test.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom01.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		'Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom02.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		'Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet skt_cali_test.Geom03.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3

		check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
		N_round = N_round + 1
		'Print "perpendicular check: ", check, " Loop: ", N_round
	
		Loop
		
	ElseIf DAT_nr = 1 Then
	
		'Double check
		check = 100
		'Integer N_round
		N_round = 0
		Do Until check < 20 And check > -20 Or N_round > 10
		VRun skt_cali_DAT1
		VGet skt_cali_DAT1.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet skt_cali_DAT1.Geom01.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_DAT1.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet skt_cali_DAT1.Geom02.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_DAT1.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet skt_cali_DAT1.Geom03.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3
		Print "P3 xyu: ", x_p3, y_p3, a_p3
		
		check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
		N_round = N_round + 1
		Print "perpendicular check: ", check, " Loop: ", N_round
	
		Loop
	
	
	EndIf
	

	
	
	If check < 20 And check > -20 Then
		Print "Correctly found"
		Double x_c, y_c
		x_c = (x_p1 + x_p3) /2
		y_c = (y_p1 + y_p3) /2
		
		Double sin_ang, sin_ang2, ang, ang2
		sin_ang = (x_p1_cam - x_p2_cam) / Sqr((x_p1_cam - x_p2_cam) * (x_p1_cam - x_p2_cam) + (y_p1_cam - y_p2_cam) * (y_p1_cam - y_p2_cam))
		
		If DAT_nr = 2 Then
			ang = Asin(sin_ang) / PI * 180
		ElseIf DAT_nr = 1 Then
			ang = 90 - Asin(sin_ang) / PI * 180
		EndIf
		


		Print "corr_center: ", x_c, y_c, ang

		JumpToSocket(DAT_nr, socket_nr)
		Print Here
		'Go Here +U(uerr)
		Print CU(Here)
		
		'Jump Here :X(x_c) :Y(y_c)
		'Go Here +U(ang)
		'Print "corr_actual: ", Here
		'Print "Correctly found"
	Else
		Print "fail, check=", check
	EndIf
	
	
	'Print "HERE: ", Here
	'Print "ori_center: ", x_ori, y_ori
	'Print "corr_center: ", x_c, y_c
	'Print P(20 + socket_nr) :Z(-132.5)
	
	'Double A_line
	
	'VGet skt_cali_test.LineFind01.Angle, A_line
	'Print A_line
	'Print CX(Here) - DF_CAMERA_OFFSET, CY(Here), x_c, y_c

	
	
Fend

Function JumpToSocket_cor(DAT_nr As Integer, socket_nr As Integer, fileNum As Integer)
	'Print
	'Print socket_nr, "**********************************************"
	
	JumpToSocket_camera(DAT_nr, socket_nr)
	UF_camera_light_ON
	
	
	Boolean Isfound1, Isfound2, Isfound3
	Boolean found
	
	Double x_p1, y_p1, a_p1, x_p2, y_p2, a_p2, x_p3, y_p3, a_p3
	Double x_ori, y_ori, a_ori
	Double x_p1_cam, y_p1_cam, x_p2_cam, y_p2_cam, x_p3_cam, y_p3_cam
	
	'VGet skt_cali_test.CameraCenter.RobotXYU, found, x_ori, y_ori, a_ori
	Double check
	Double edge_diff, edge1, edge2
	Boolean quality_len, quality_pen
	Integer N_round
	If DAT_nr = 2 And socket_nr < 19 Then
	
		
		check = 100
		edge_diff = 100
		N_round = 0
		quality_len = False
		quality_pen = False
		Do Until quality_pen And quality_len Or N_round > 20
		VRun skt_cali_test
		VGet skt_cali_test.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom01.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		'Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_test.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom02.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		'Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_test.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet skt_cali_test.Geom03.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3
		If Isfound1 And Isfound2 And Isfound3 Then
			check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
			edge1 = (x_p1 - x_p2) * (x_p1 - x_p2) + (y_p1 - y_p2) * (y_p1 - y_p2)
			edge2 = (x_p2 - x_p3) * (x_p2 - x_p3) + (y_p2 - y_p3) * (y_p2 - y_p3)
			edge_diff = Abs(edge1 - edge2)
	
			If edge_diff > 900 And edge_diff < 1000 Then
				quality_len = True
			EndIf

			If check < 20 Or check > -20 Then
				quality_pen = True
			EndIf

		EndIf
		
		N_round = N_round + 1
		Print "perpendicular check: ", check, " Loop: ", N_round, " edge_diff = ", edge_diff, edge1, edge2
	
		Loop
		
	ElseIf DAT_nr = 1 And socket_nr < 19 Then
	
		'Double check
		check = 100
		edge_diff = 100
		'Integer N_round
		N_round = 0
		quality_len = False
		quality_pen = False
 
		Do Until quality_pen And quality_len Or N_round > 20
		VRun skt_cali_DAT1
		VGet skt_cali_DAT1.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet skt_cali_DAT1.Geom01.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet skt_cali_DAT1.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet skt_cali_DAT1.Geom02.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet skt_cali_DAT1.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet skt_cali_DAT1.Geom03.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3
		Print "P3 xyu: ", x_p3, y_p3, a_p3
		If Isfound1 And Isfound2 And Isfound3 Then
			check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
			edge1 = (x_p1 - x_p2) * (x_p1 - x_p2) + (y_p1 - y_p2) * (y_p1 - y_p2)
			edge2 = (x_p2 - x_p3) * (x_p2 - x_p3) + (y_p2 - y_p3) * (y_p2 - y_p3)
			edge_diff = Abs(edge1 - edge2)

				If edge_diff > 900 And edge_diff < 1000 Then
					quality_len = True
				EndIf
				
				If check < 20 Or check > -20 Then
					quality_pen = True
				EndIf

		EndIf
		N_round = N_round + 1
		Print "perpendicular check: ", check, " Loop: ", N_round, " edge_diff = ", edge_diff, edge1, edge2
	
		Loop
	
	ElseIf DAT_nr = 1 And socket_nr > 19 Then
			'Double check
		check = 100
		edge_diff = 100
		'Integer N_round
		N_round = 0
		quality_len = False
		quality_pen = False
 
		Do Until quality_pen And quality_len Or N_round > 20
		VRun sktCD_cali_DAT1
		VGet sktCD_cali_DAT1.Geom01.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet sktCD_cali_DAT1.Geom01.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet sktCD_cali_DAT1.Geom02.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet sktCD_cali_DAT1.Geom02.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet sktCD_cali_DAT1.Geom03.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet sktCD_cali_DAT1.Geom03.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3
		Print "P3 xyu: ", x_p3, y_p3, a_p3
		If Isfound1 And Isfound2 And Isfound3 Then
			check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
			edge1 = (x_p1 - x_p2) * (x_p1 - x_p2) + (y_p1 - y_p2) * (y_p1 - y_p2)
			edge2 = (x_p2 - x_p3) * (x_p2 - x_p3) + (y_p2 - y_p3) * (y_p2 - y_p3)
			edge_diff = Abs(edge1 - edge2)

			If edge_diff < 100 Then
				quality_len = True
			EndIf

			If check < 40 Or check > -40 Then
				quality_pen = True
			EndIf
		EndIf
		N_round = N_round + 1
		Print "perpendicular check: ", check, " Loop: ", N_round, " edge_diff = ", edge_diff, edge1, edge2
	
		Loop
		
	ElseIf DAT_nr = 2 And socket_nr > 19 Then
			'Double check
		check = 100
		edge_diff = 100
		'Integer N_round
		N_round = 0
		quality_len = False
		quality_pen = False
 
		Do Until quality_pen And quality_len Or N_round > 20
		VRun sktCD_cali_DAT1
		VGet sktCD_cali_DAT1.Geom02.RobotXYU, Isfound1, x_p1, y_p1, a_p1
		VGet sktCD_cali_DAT1.Geom02.CameraXYU, Isfound1, x_p1_cam, y_p1_cam, a_p1
		Print "P1 xyu: ", x_p1, y_p1, a_p1
		VGet sktCD_cali_DAT1.Geom03.RobotXYU, Isfound2, x_p2, y_p2, a_p2
		VGet sktCD_cali_DAT1.Geom03.CameraXYU, Isfound2, x_p2_cam, y_p2_cam, a_p2
		Print "P2 xyu: ", x_p2, y_p2, a_p2
		VGet sktCD_cali_DAT1.Geom04.RobotXYU, Isfound3, x_p3, y_p3, a_p3
		VGet sktCD_cali_DAT1.Geom04.CameraXYU, Isfound3, x_p3_cam, y_p3_cam, a_p3
		Print "P3 xyu: ", x_p3, y_p3, a_p3
		If Isfound1 And Isfound2 And Isfound3 Then
			check = (x_p1 - x_p2) * (x_p3 - x_p2) - (y_p1 - y_p2) * (y_p3 - y_p2)
			edge1 = (x_p1 - x_p2) * (x_p1 - x_p2) + (y_p1 - y_p2) * (y_p1 - y_p2)
			edge2 = (x_p2 - x_p3) * (x_p2 - x_p3) + (y_p2 - y_p3) * (y_p2 - y_p3)
			edge_diff = Abs(edge1 - edge2)

			If edge_diff < 100 Then
				quality_len = True
			EndIf

			If check < 40 Or check > -40 Then
				quality_pen = True
			EndIf
		EndIf
		N_round = N_round + 1
		Print "perpendicular check: ", check, " Loop: ", N_round, " edge_diff = ", edge_diff, edge1, edge2
	
		Loop
	
	EndIf
	
	UF_camera_light_OFF
	
	If quality_pen And quality_len Then
		Print "Correctly found"
		Double x_c, y_c
		x_c = (x_p1 + x_p3) /2
		y_c = (y_p1 + y_p3) /2
		
		Double sin_ang, sin_ang2, ang, ang2
		sin_ang = (x_p1_cam - x_p2_cam) / Sqr((x_p1_cam - x_p2_cam) * (x_p1_cam - x_p2_cam) + (y_p1_cam - y_p2_cam) * (y_p1_cam - y_p2_cam))


		If DAT_nr = 2 Then
			ang = Asin(sin_ang) / PI * 180
		'	If ang > 180 Then
		'		ang = ang - 180
		'	EndIf
		ElseIf DAT_nr = 1 Then
			ang = 90 - Asin(sin_ang) / PI * 180
		EndIf

		Print "corr_center: ", x_c, y_c, ang

		JumpToSocket(DAT_nr, socket_nr)
		'Print CU(Here)
		
		Jump Here :X(x_c) :Y(y_c)
		Go Here +U(ang)
		Print "corr_actual: ", Here
	Else
		JumpToSocket(DAT_nr, socket_nr)
		Print "socket correction fail"
	EndIf

	Print #fileNum, ",", DAT_nr, ",", socket_nr, ",", x_c, ",", y_c, ",", CU(Here),
	
	
	
Fend

Function Socket_height_calibration(DAT_nr As Integer, socket_nr As Integer, fileNum As Integer)

	JumpToSocket_cor(DAT_nr, socket_nr, fileNum)
	Speed 1
	Accel 1, 1
	
	Double h_tot, h_step
	h_tot = 10
	h_step = 1
	Go Here -Z(h_tot)
	Do Until isContactSensorTouches Or h_tot > 17
		Go Here -Z(h_step)
    	Wait 0.5
    	h_tot = h_tot + h_step
    Loop
    Print DAT_nr, socket_nr, h_tot
    Socket_height_calibration = h_tot
    Print #fileNum, ",", h_tot
	SetSpeed
	
Fend
Function Socket_height_calibration_all()
	String skt_calibration$
	skt_calibration$ = RTS_DATA + "skt_calibration/"
	String ts$
	ts$ = FmtStr$(Date$ + " " + Time$, "yyyymmddhhnnss")
	String fname$
	fname$ = ts$ + "socket_calibration.csv"
	Print fname$
	Integer fileNum
	fileNum = FreeFile
	AOpen skt_calibration$ + fname$ As #fileNum
	
	'Socket_height_calibration(1, 1, fileNum)
	'Socket_height_calibration(1, 2, fileNum)
	'Socket_height_calibration(1, 3, fileNum)
	'Socket_height_calibration(1, 4, fileNum)
	'Socket_height_calibration(1, 5, fileNum)
	'Socket_height_calibration(1, 6, fileNum)
	'Socket_height_calibration(1, 7, fileNum)
	'Socket_height_calibration(1, 8, fileNum)
	
	'Socket_height_calibration(2, 11, fileNum)
	'Socket_height_calibration(2, 12, fileNum)
	'Socket_height_calibration(2, 13, fileNum)
	'Socket_height_calibration(2, 14, fileNum)
	'Socket_height_calibration(2, 15, fileNum)
	'Socket_height_calibration(2, 16, fileNum)
	'Socket_height_calibration(2, 17, fileNum)
	'Socket_height_calibration(2, 18, fileNum)
	
	'Socket_height_calibration(1, 21, fileNum)
	'Socket_height_calibration(1, 22, fileNum)
	
	'Socket_height_calibration(2, 21, fileNum)
	Socket_height_calibration(2, 22, fileNum)
	
	
	Close #fileNum
	
Fend
Function Get_asic_position(ByRef P_xyu() As Double) As Integer
	'Integer i, fileNum
	VRun ChipBottom_Analy
	Boolean ret_found
	Double camera_X, camera_Y, camera_X1, camera_X2
	Double X_0, Y_0, U_0


	'VGet ChipBottom_Analy.Final.RobotXYU, ret_found, ret_X, ret_Y, ret_U
	VGet ChipBottom_Analy.Final.Found, ret_found
	If ret_found Then

		VGet ChipBottom_Analy.CameraCenter.CameraX, camera_X
		VGet ChipBottom_Analy.CameraCenter.CameraY, camera_Y

			
		VGet ChipBottom_Analy.Final.CameraX, X_0
		VGet ChipBottom_Analy.Final.CameraY, Y_0
		VGet ChipBottom_Analy.Final.Angle, U_0
		VGet ChipBottom_Analy.Final.CameraX1, camera_X1
		VGet ChipBottom_Analy.Final.CameraX2, camera_X2

		'Print #fileNum, ",", ret_found,
		Print "camera ", camera_X, ",", camera_Y
		Print "square", X_0, ",", Y_0, ",", U_0, ",", camera_X1, ",", camera_X2
		
		
		P_xyu(1) = X_0
		P_xyu(2) = Y_0
		P_xyu(3) = U_0
		Get_asic_position = 0

	Else
		
		Get_asic_position = 1
		Print "***ERROR ", 1
        Exit Function
		
	EndIf
	
Fend

Function Get_CD_position(ByRef P_xyu() As Double) As Integer

	'Integer i, fileNum
	VRun CB_CD_Analy
	Boolean ret_found
	Double camera_X, camera_Y, camera_X1, camera_X2
	Double X_0, Y_0, U_0


	'VGet ChipBottom_Analy.Final.RobotXYU, ret_found, ret_X, ret_Y, ret_U
	VGet CB_CD_Analy.Final.Found, ret_found
	If ret_found Then

		VGet CB_CD_Analy.CameraCenter.CameraX, camera_X
		VGet CB_CD_Analy.CameraCenter.CameraY, camera_Y

			
		VGet CB_CD_Analy.Final.CameraX, X_0
		VGet CB_CD_Analy.Final.CameraY, Y_0
		VGet CB_CD_Analy.Final.Angle, U_0
		VGet CB_CD_Analy.Final.CameraX1, camera_X1
		VGet CB_CD_Analy.Final.CameraX2, camera_X2

		'Print #fileNum, ",", ret_found,
		Print "camera ", camera_X, ",", camera_Y
		Print "square", X_0, ",", Y_0, ",", U_0, ",", camera_X1, ",", camera_X2
		
		
		P_xyu(1) = X_0
		P_xyu(2) = Y_0
		P_xyu(3) = U_0

	Else
		
		Get_CD_position = 1
		Print "***ERROR ", 1
        Exit Function
		
	EndIf
	
Fend
Function ChipBottomAnaly_CD(id$ As String, ByRef idx() As Integer, ByRef res() As Double) As Integer
	Integer i, fileNum
	ChipBottomAnaly_CD = 0
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
	ElseIf tgt_pallet_nr > 0 And tgt_pallet_nr > 10 And tgt_col_nr > 0 And tgt_col_nr <= TRAY_CD_NCOLS And tgt_row_nr > 0 And tgt_row_nr <= TRAY_CD_NROWS Then
		tgt_x0 = tray_CD_X(tgt_pallet_nr - 10, tgt_col_nr, tgt_row_nr)
		tgt_y0 = tray_CD_Y(tgt_pallet_nr - 10, tgt_col_nr, tgt_row_nr)
		tgt_u0 = tray_CD_U(tgt_pallet_nr - 10, tgt_col_nr, tgt_row_nr)
		dst_U = CU(Pallet(tgt_pallet_nr, tgt_col_nr, tgt_row_nr))
	Else
		ChipBottomAnaly_CD = 100
		Exit Function
	EndIf
	
	
	UF_camera_light_ON
	Wait 0.2
	String pict_fname$
	'UF_take_picture(chip_SN$, ByRef pict_fname_0$)
	pict_fname$ = UF_take_picture$(id$ + "_01")
    Print #fileNum, ",", pict_fname$,
    
 
    Double P_xyu_0(3)
    Print "1ST:"
    ChipBottomAnaly_CD = Get_CD_position(ByRef P_xyu_0())
    If ChipBottomAnaly_CD <> 0 Then
    	Exit Function
    EndIf
 
	Go Here -U(180)
	Wait 0.2

	pict_fname$ = UF_take_picture$(id$ + "_02")
	Print #fileNum, ",", pict_fname$,
	Print "2nd:"
    Double P_xyu_180(3)
    ChipBottomAnaly_CD = Get_CD_position(ByRef P_xyu_180())
        If ChipBottomAnaly_CD <> 0 Then
    	Exit Function
    EndIf
    
    Double X_tool, Y_tool, X_0, Y_0, U_0
    

    X_tool = 0.5 * (P_xyu_0(1) + P_xyu_180(1))
	Y_tool = 0.5 * (P_xyu_0(2) + P_xyu_180(2))
	X_0 = P_xyu_0(1)
	Y_0 = P_xyu_0(2)
	U_0 = P_xyu_0(3)
	
	Print "tool: ", X_tool, Y_tool, U_0
	
	res(9) = X_tool
	res(10) = Y_tool
	
	Print #fileNum, ",", X_tool, ",", Y_tool,
	
	res(11) = P_xyu_0(1) - X_tool
	res(12) = P_xyu_0(2) - Y_tool
	
	Print #fileNum, ",", res(11), ",", res(12),

		' record the chip position from the source
		Double src_x0, src_y0, src_u0
		' source: 1 - pallet, 2 - socket
		Integer src
		Print src_pallet_nr, src_col_nr, src_row_nr
		If src_pallet_nr > 0 And src_pallet_nr > 10 And src_col_nr > 0 And src_col_nr <= TRAY_CD_NCOLS And src_row_nr > 0 And src_row_nr <= TRAY_CD_NROWS Then
			src_x0 = tray_CD_X(src_pallet_nr - 10, src_col_nr, src_row_nr)
			src_y0 = tray_CD_Y(src_pallet_nr - 10, src_col_nr, src_row_nr)
			src_u0 = tray_U(src_pallet_nr - 10, src_col_nr, src_row_nr)
			src = 1
		ElseIf src_DAT_nr > 0 And src_DAT_nr <= 2 And src_socket_nr > 0 And src_socket_nr <= NSOCKETS Then
			src_x0 = DAT_X(src_DAT_nr, src_socket_nr)
			src_y0 = DAT_Y(src_DAT_nr, src_socket_nr)
			src_u0 = DAT_U(src_DAT_nr, src_socket_nr)
			src = 2
		Else
			ChipBottomAnaly_CD = 101
			Exit Function
		EndIf

		If src_x0 = 0 And src_y0 = 0 And src_u0 = 0 Then
			Print "Recording position of the source: ", res(11), " ", res(12), " ", U_0
			If src = 1 Then
				tray_CD_X(src_pallet_nr - 10, src_col_nr, src_row_nr) = res(11)
				tray_CD_Y(src_pallet_nr - 10, src_col_nr, src_row_nr) = res(12)
				tray_CD_U(src_pallet_nr - 10, src_col_nr, src_row_nr) = U_0
			ElseIf src = 2 Then
				DAT_X(src_DAT_nr, src_socket_nr) = res(11)
				DAT_Y(src_DAT_nr, src_socket_nr) = res(12)
				DAT_U(src_DAT_nr, src_socket_nr) = U_0
			Else
				ChipBottomAnaly_CD = 102
				Exit Function
			EndIf
		EndIf
	
	Print "current angle: ", CU(Here)
		' Change handeness to the target 
	If tgt_pallet_nr = 1 Or tgt_DAT_nr = 1 Or tgt_pallet_nr = 11 Then
    	Jump P_camera :U(dst_U) /R
    ElseIf tgt_pallet_nr = 2 Or tgt_DAT_nr = 2 Or tgt_pallet_nr = 12 Then
    	Jump P_camera :U(dst_U) /L
    EndIf
    
    Print "Dest angle: ", dst_U
    
    Print "3rd:"
    ChipBottomAnaly_CD = Get_CD_position(ByRef P_xyu_0())
        If ChipBottomAnaly_CD <> 0 Then
    	Exit Function
    EndIf
    
	Go Here -U(180)
	Wait 0.2
	
    Print "4th:"
    ChipBottomAnaly_CD = Get_CD_position(ByRef P_xyu_180())
        If ChipBottomAnaly_CD <> 0 Then
    	Exit Function
    EndIf
    
    X_tool = 0.5 * (P_xyu_0(1) + P_xyu_180(1))
	Y_tool = 0.5 * (P_xyu_0(2) + P_xyu_180(2))
	X_0 = P_xyu_0(1)
	Y_0 = P_xyu_0(2)
	U_0 = P_xyu_0(3)
	
	Print "tool2: ", X_tool, Y_tool
    
	Double d_U
	d_U = tgt_u0 - U_0
	res(16) = d_U
	Wait 0.2
	
	Double d_X, d_Y
	d_X = tgt_x0 - (X_0 - X_tool)
	d_Y = tgt_y0 - (Y_0 - Y_tool)
    
    Print #fileNum, ",", d_X, ",", d_Y, ",", d_U,
    Print d_X, ",", d_Y, ",", d_U

	res(20) = d_X
	res(21) = d_Y
	Print Here
	Print X_0, Y_0, U_0
	Print X_tool, Y_tool
	
	d_X = X_0 - X_tool
	d_Y = Y_0 - Y_tool
	
	Go Here +X(d_X) +Y(d_Y) +U(U_0)
	
	Print "check:"
	Double P_xyu_check(3)
    ChipBottomAnaly_CD = Get_CD_position(ByRef P_xyu_check())
        If ChipBottomAnaly_CD <> 0 Then
    	Exit Function
    EndIf
    
    Print "check: ", P_xyu_check(1), P_xyu_check(2), P_xyu_check(3)
    pict_fname$ = UF_take_picture$(id$ + "_check")
    
    Integer status
	status = PinsAnaly_CD(id$)
	Print #fileNum, ",", status,
	If status <> 0 Then
		ChipBottomAnaly_CD = status
	EndIf
	Print "pin_analy: error code: ", status, ChipBottomAnaly_CD
    
    Integer If_key_check
    If_key_check = 0 'key difference is small, seeking OCR checking instead
    If If_key_check = 1 Then
    	Boolean res_1, res_2, res_3, res_4
	If tgt_DAT_nr = 1 Then
		Print "Checking ASIC key"
		'VSet key_check_1.ImageFile, pict_fname$
		VRun key_check_CD
		
		VGet key_check_CD.Blob01.Found, res_1
		VGet key_check_CD.Blob02.Found, res_2
		VGet key_check_CD.Blob03.Found, res_3
		VGet key_check_CD.Blob04.Found, res_4
		
		If Not (res_1 And (Not res_2) And res_3 And res_4) Then
			Print "***ERROR! Failed to determine the key position of the ASIC"
			ChipBottomAnaly_CD = 7
			Exit Function
		EndIf
	ElseIf tgt_DAT_nr = 2 Then
		Print "Checking ASIC key for codata dat2 Not ready yet!!!!"
'		Print "XN: key check file:", pict_fname$
		'VSet key_check.ImageFile, pict_fname$
'		VRun key_check
		
'		VGet key_check.Blob01.Found, res_1
'		VGet key_check.Blob02.Found, res_2
'		VGet key_check.Blob03.Found, res_3
'		VGet key_check.Blob04.Found, res_4
		
'		If Not (res_1 And res_2 And res_3 And (Not res_4)) Then
'			Print "***ERROR! Failed to determine the key position of the ASIC"
'			ChipBottomAnaly = 7
'			Exit Function
'		EndIf
	EndIf
	
    EndIf
    
	
    UF_camera_light_OFF
    
Fend


Function PinsAnaly_CD(id$ As String) As Integer
	
	PinsAnaly_CD = 302
	String pict_fname$
	
	'VRun pins_analy_CD
	
	Integer status1, status2, status3, status4
	
	status1 = PinsRowAnaly_CD("BlobTop")

	
	status2 = PinsRowAnaly_CD("BlobBottom")


	status3 = PinsRowAnaly_CD("BlobLeft")


	status4 = PinsRowAnaly_CD("BlobRight")
	
	Print status1, status2, status3, status4
	
	If status1 = 0 And status2 = 0 Then
		Go Here +X(26.3)
		Wait 1
		pict_fname$ = UF_take_picture$(id$ + "_bl")
		'VRun pins_analy_CD2
		status3 = PinsRowAnaly_CD("BlobLeft")

		Go Here -X(26.3)
		Go Here -X(26.3)
		Wait 1
		pict_fname$ = UF_take_picture$(id$ + "_br")
		'VRun pins_analy_CD
		status4 = PinsRowAnaly_CD("BlobRight")
		Go Here +X(26.3)
		If status3 = 0 And status4 = 0 Then
			PinsAnaly_CD = 0
		Else
			PinsAnaly_CD = 302
		EndIf
		
		
	ElseIf status3 = 0 And status4 = 0 Then
		Go Here +Y(26.3)
		Wait 1
		'VRun pins_analy_CD
		pict_fname$ = UF_take_picture$(id$ + "_bt")
		status1 = PinsRowAnaly_CD("BlobTop")
		Go Here -Y(26.3)
		Go Here -Y(26.3)
		Wait 1
		'VRun pins_analy_CD
		pict_fname$ = UF_take_picture$(id$ + "_bb")
		status2 = PinsRowAnaly_CD("BlobBottom")
		Go Here +Y(26.3)
		If status1 = 0 And status2 = 0 Then
			PinsAnaly_CD = 0
		Else
			PinsAnaly_CD = 302
		EndIf

	Else
		PinsAnaly_CD = 302
			
	EndIf

	
Fend

Function PinsRowAnaly_CD(name$ As String) As Integer
	
	PinsRowAnaly_CD = 0
	VRun pins_analy_CD
	Boolean passed
	Integer nFound, i
	Double x, y, area, xold, yold
		
	VGet pins_analy_CD.name$.Passed, passed
	'Print passed
	If Not passed Then
		Print "PinsAnaly_CD " + name$ + " failed!"
		'Print #fileNum, " failed"
		PinsRowAnaly_CD = 301
		Exit Function
	EndIf

	VGet pins_analy_CD.name$.NumberFound, nFound
	'Print #fileNum, name$, ",", nFound,
	If nFound <> 54 Then
		PinsRowAnaly_CD = 302
	EndIf

'	For i = 1 To nFound
'		VSet pins_analy.name$.CurrentResult, i
'		VGet pins_analy.name$.CameraX, x
'		VGet pins_analy.name$.CameraY, y
'		VGet pins_analy.name$.Area, area
'		Print #fileNum, ",", x, ",", y, ",", area,
'		xold = x
'		yold = y
'		If i > 1 And Abs(x - xold) > 0.05 Then
'			Print "*ERROR! Bent pin found in " + name$
'			PinsRowAnaly = 400 + i
'		EndIf
'	Next i
'	Print #fileNum, " "
	
Fend
Function PinsRowAnaly_CD2(name$ As String) As Integer
	
	PinsRowAnaly_CD2 = 0
	VRun pins_analy_CD2
	Boolean passed
	Integer nFound, i
	Double x, y, area, xold, yold
		
	VGet pins_analy_CD2.name$.Passed, passed
	Print passed
	If Not passed Then
		Print "PinsAnaly_CD2 " + name$ + " failed!"
		'Print #fileNum, " failed"
		PinsRowAnaly_CD2 = 301
		Exit Function
	EndIf

	VGet pins_analy_CD2.name$.NumberFound, nFound
	'Print #fileNum, name$, ",", nFound,
	If nFound <> 54 Then
		PinsRowAnaly_CD2 = 302
	EndIf

'	For i = 1 To nFound
'		VSet pins_analy.name$.CurrentResult, i
'		VGet pins_analy.name$.CameraX, x
'		VGet pins_analy.name$.CameraY, y
'		VGet pins_analy.name$.Area, area
'		Print #fileNum, ",", x, ",", y, ",", area,
'		xold = x
'		yold = y
'		If i > 1 And Abs(x - xold) > 0.05 Then
'			Print "*ERROR! Bent pin found in " + name$
'			PinsRowAnaly = 400 + i
'		EndIf
'	Next i
'	Print #fileNum, " "
	
Fend
Function OCR() As Boolean
	'COLDATA
	Integer N_found, i
	String char$
	Boolean C_f, O_f, L_f, D_f, A_f, T_f

	C_f = False
	O_f = False
	L_f = False
	D_f = False
	A_f = False
	T_f = False
	VRun OCRtest
	VGet OCRtest.Ocr01.NumberFound, N_found
	For i = 1 To N_found 'get robot coords
    	VGet OCRtest.Ocr01.Text(i), char$
    	Print char$
    	If UCase$(char$) = "C" Then
    		Print char$, "C found"
    		C_f = True
    	EndIf
    	If UCase$(char$) = "O" Then
    	Print char$, "O found"
    		O_f = True
    	EndIf
    	If UCase$(char$) = "L" Then
    	Print char$, "L found"
    		L_f = True
    	EndIf
    	If UCase$(char$) = "D" Then
    	Print char$, "D found"
    		D_f = True
    	EndIf
    	If UCase$(char$) = "A" Then
    	Print char$, "A found"
    		A_f = True
    	EndIf
    	If UCase$(char$) = "T" Then
    	Print char$, "T found"
    		T_f = True
    	EndIf
    	
  	Next i

	OCR = C_f And O_f And L_f And D_f And A_f And T_f
	Print OCR
	If OCR Then
		Print "pass OCR"
	Else
		Print "fail OCR"
	EndIf
Fend
Function Tray_position_calibration(pallet_nr As Integer, corner As Integer, fileNum As Integer) As Integer

	Integer col, row, max_col, max_row
	
	If pallet_nr > 10 Then
		max_col = TRAY_CD_NCOLS
		max_row = TRAY_CD_NROWS
	Else
		max_col = TRAY_NCOLS
		max_row = TRAY_NROWS
	EndIf

	If corner = 1 Then
		col = 1
		row = 1
	ElseIf corner = 2 Then
		col = max_col
		row = 1
	ElseIf corner = 3 Then
		col = 1
		row = max_row
	ElseIf corner = 4 Then
		col = max_col
		row = max_row
	Else
		Print "corner number 1-4"
	EndIf
	
	
	If Not isPressureOk Then
		RTS_error(fileNum, "Bad pressure")
        Tray_position_calibration = -2
		Exit Function
	EndIf
				
	If Not isVacuumOk Then
		RTS_error(fileNum, "Bad vacuum")
        Tray_position_calibration = -3
		Exit Function
	EndIf
	
	JumpToTray(pallet_nr, col, row)
	
	If Not PickupFromTray Then
		RTS_error(fileNum, "Can't pickup a chip from tray ")
        Tray_position_calibration = -4
		Exit Function
	EndIf
	
	JumpToCamera
	Double xyu_cor(3)
	Integer status
		If pallet_nr > 10 Then
		status = ChipBottom_4corr_CD(ByRef xyu_cor())
	Else
		status = ChipBottom_4corr(ByRef xyu_cor())
		'Print "XN: After ChipBottomAnaly ", Here
	EndIf
	
	JumpToTray(pallet_nr, col, row)
	DropToTray
	
	Go Here +X(xyu_cor(1)) +Y(xyu_cor(2)) +U(xyu_cor(3))
	
	Print "correct tray position"
	Print Here
    Print #fileNum, ",", pallet_nr, ",", row, ",", col, ",", Here

	
Fend

Function ChipBottom_4corr_CD(ByRef P_xyu() As Double) As Integer
	UF_camera_light_ON
	Wait 0.2
 
    Double P_xyu_0(3)
    Print "1ST:"
    ChipBottom_4corr_CD = Get_CD_position(ByRef P_xyu_0())
    If ChipBottom_4corr_CD <> 0 Then
    	Exit Function
    EndIf
 
	Go Here -U(180)
	Wait 0.2

	Print "2nd:"
    Double P_xyu_180(3)
    ChipBottom_4corr_CD = Get_CD_position(ByRef P_xyu_180())
        If ChipBottom_4corr_CD <> 0 Then
    	Exit Function
    EndIf
    
    Double X_tool, Y_tool, X_0, Y_0, U_0
    

    X_tool = 0.5 * (P_xyu_0(1) + P_xyu_180(1))
	Y_tool = 0.5 * (P_xyu_0(2) + P_xyu_180(2))
	X_0 = P_xyu_0(1)
	Y_0 = P_xyu_0(2)
	U_0 = P_xyu_0(3)
	
	Print "tool: ", X_tool, Y_tool, U_0
	
		
	P_xyu(1) = P_xyu_0(1) - X_tool
	P_xyu(2) = P_xyu_0(2) - Y_tool
	P_xyu(3) = -U_0
	
	ChipBottom_4corr_CD = 0
		'xyu_cor(1) = res(11)
		'xyu_cor(2) = res(12)
		'xyu_cor(3) = res(16)
		'Go Here +X(xyu_cor(1)) +Y(xyu_cor(2)) +U(xyu_cor(2))
		'Print "correct tray position"
		'Print Here
Fend
Function ChipBottom_4corr(ByRef P_xyu() As Double) As Integer
	UF_camera_light_ON
	Wait 0.2
 
    Double P_xyu_0(3)
    Print "1ST:"
    ChipBottom_4corr = Get_asic_position(ByRef P_xyu_0())
    If ChipBottom_4corr <> 0 Then
    	Exit Function
    EndIf
 
	Go Here -U(180)
	Wait 0.2

	Print "2nd:"
    Double P_xyu_180(3)
    ChipBottom_4corr = Get_asic_position(ByRef P_xyu_180())
        If ChipBottom_4corr <> 0 Then
    	Exit Function
    EndIf
    
    Double X_tool, Y_tool, X_0, Y_0, U_0
    

    X_tool = 0.5 * (P_xyu_0(1) + P_xyu_180(1))
	Y_tool = 0.5 * (P_xyu_0(2) + P_xyu_180(2))
	X_0 = P_xyu_0(1)
	Y_0 = P_xyu_0(2)
	U_0 = P_xyu_0(3)
	
	Print "tool: ", X_tool, Y_tool, U_0
	
		
	P_xyu(1) = P_xyu_0(1) - X_tool
	P_xyu(2) = P_xyu_0(2) - Y_tool
	P_xyu(3) = -U_0
	
	ChipBottom_4corr = 0
		'xyu_cor(1) = res(11)
		'xyu_cor(2) = res(12)
		'xyu_cor(3) = res(16)
		'Go Here +X(xyu_cor(1)) +Y(xyu_cor(2)) +U(xyu_cor(2))
		'Print "correct tray position"
		'Print Here
Fend

Function Tray_calibration_all()
	String tray_calibration$
	tray_calibration$ = RTS_DATA + "tray_calibration/"
	String ts$
	ts$ = FmtStr$(Date$ + " " + Time$, "yyyymmddhhnnss")
	String fname$
	fname$ = ts$ + "tray_calibration.csv"
	Print fname$
	Integer fileNum
	fileNum = FreeFile
	AOpen tray_calibration$ + fname$ As #fileNum
	
	Tray_position_calibration(1, 1, fileNum)
	Tray_position_calibration(1, 2, fileNum)
	Tray_position_calibration(1, 3, fileNum)
	Tray_position_calibration(1, 4, fileNum)
	Tray_position_calibration(2, 1, fileNum)
	Tray_position_calibration(2, 2, fileNum)
	Tray_position_calibration(2, 3, fileNum)
	Tray_position_calibration(2, 4, fileNum)
	
	Tray_position_calibration(11, 1, fileNum)
	Tray_position_calibration(11, 2, fileNum)
	Tray_position_calibration(11, 3, fileNum)
	Tray_position_calibration(11, 4, fileNum)
	Tray_position_calibration(12, 1, fileNum)
	Tray_position_calibration(12, 2, fileNum)
	Tray_position_calibration(12, 3, fileNum)
	Tray_position_calibration(12, 4, fileNum)
	
	Close #fileNum
	
Fend

