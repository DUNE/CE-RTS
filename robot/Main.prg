' RTS Demo Program Version 2024-05-19
' Questions? dean.shooltz@gmail.com or bromberg@msu.edu

' ****************************
' *** Overall Description: ***
' ****************************

' The RTS motion system is based on a limited set of fixed points.
' Refer to a graphic map of the Points.  Text version below:
' -- Points 0 to 17 refer to the sockets on the Left DAT board
' -- Points 18 to 35 refer to the sockets on the Left DAT board
' -- Points 36-39 define Tray1, and Points 40-43 define Tray2
' -- Point 44 (Camera)
' -- Point 45 (Park) 
' And a limited set of motion operations (Tray to Socket, Socket to Tray, etc)


' *****************
' *** VARIABLES ***
' *****************

' It is expected that the DAT board points may change during steady-state operation:
' -- Cryo cycling might lead to some movement of the DAT board
' To account for this, an array of positional offsets is maintained.  
' These are updated whenever a chip is removed from a socket

' Define an array of offsets for the 36 Socket positions
' The offsets are added to the fixed points to account for the positional error of the sockets
' Define this array as Global Preserve in order to save this information in the battery-backed
' static RAM in the Epson RC700D cpntroller
' NOTE: offsets are only maintained for the ASIC test sockets, not for the Trays, Camera, or Park position
' NOTE: Be careful- the battery-backed static RAM will lose info if the battery gets weak
' THUS: Back up the offset data up elsewhere?
' Alternatively- just use the UpdatePickPoint function to reset these values if they are ever lost
Global Preserve Real g_Offset(35, 3)

' Also define an array of Boolean to indicate whether or not the Offsets have been set.
' NOTE: we can't safely install a chip without knowing the positional offsets to get it into the socket
Global Preserve Boolean g_OffsetOK(35)

Real Xcorr, Ycorr		'Camera-reported real-world coordinate position of the corner of the ASIC (using Corr01 center coordinates)
Real Qscore				'Quality measure reported by the Image Recognition, maybe useful for diagnostics
Integer CorrFailCount	'counter of failures to find the corners with Corr01
Integer FindStat		' Status of the Image recognition: 
' FindStat = 1 => A Corr-based measurement was used to generate the tool offsets
' FindStat = 0 => The ASIC image recognition failed
' FindStat = -1 => The computed offsets are out of range (hardcoded at 3 mm for now)

Real Xoff, Yoff, Uoff	' These are the offsets reported from the ASIC finding routine FindChip4

Real Zset_SP		'The Starting Point in Z for SocketPlace and SocketPick
Real Zpcs  			'The detected Z position when using the Payload Contact Sensor
Real Zrel			'The relative distance in z to find the PCS - useful for diagnostics
Boolean Zfound		'TRUE means we found a PCS trip, FALSE means that we did not find PCS before end of downward motion

Integer SocketPickStatus	' Status returned form the SocketPick function
' SocketPickStatus = 1 	=> Apparently a success
' SocketPickStatus = -1	=> The Pressure switch indicated low pressure
' SocketPickStatus = -2	=> The Vacuum switch indicated low vacuum after pick operation

' Additionally, there are some variables best described in the context of the movement functions below



' *****************
' *** FUNCTIONS ***
' *****************

'UpdatePickPoint : Very important, needed to set up the positional offsets

'UpdateTrayPoints : Runs UpdatePickPoint on the four points that define a JEDEC tray as a "Pallet" in the Epson RC+ software, then redefine the Pallet


' The set of four chip movement functions 
' All chip movements are performed by the following four functions:
' NOTE: T2S and S2S will refuse to move chips unless the g_OffsetOK flag for the destination is set by the UpdatePickPoint function

' Function	   Action				Usage
' --------	---------------		-------------------------------------
'   S2T 	Socket to Tray		S2T (SocketA, TrayB, TrayPosB)
'   T2S 	Tray to Socket		T2S (TrayA, TrayPosA, SocketB)
'   S2S 	Socket to Socket	S2S (SocketA, SocketB)
'   T2T 	Tray to Tray		T2T (TrayA, TrayPosA, TrayB, TrayPosB)
'   T2Tfast	Tray to Tray		T2T (TrayA, TrayPosA, TrayB, TrayPosB)  (Note: this version skips the visit to the camera)


' ****************************************************************************************
' *** Other Functions, generally only called from within the functions described above ***
' ****************************************************************************************

'Initialize and FinishUp : Collections of startup and closing tasks

'TrayPick, TrayPlace, SocketPick, SocketPlace : The low-level motions for pick and place

'ShowOffsets, CheckOffsets, ClearOffsets : Tools for working with the array of positional offsets

'CreateDAT1 : Prototype of a utility to define the DAT points using the DAT board geometry

'MoveSpeed, PickSpeed : consolidating the calls for different speeds

'CameraViewQ1, CameraViewQ3 : Calls the image recognition routines on Epson CV2 vision processor in two different quadrants

'FindChip4 : uses the coordinates from the Epson CV2 tp determine chip center, J3 axis center, and the vector between these two points


' ****************************
' *** Functions Wish List ***
' ****************************

' -- Some function to store the 46 points defining the RTS as higher-numbered Points (index + 100?)
' -- This gives us a reference set of points for setting up a new system based on another previously-built system

' -- Function to drive the down-facing camera over a chip and save image to disk


' ***********************************
' *** Robot Controller I/O lines" ***
' ***********************************
' Out 9  : Vacuum pump relay
' Out 10 : Vacuum solenoid Valve
' Out 11 : Socket actuator solenoid valve
' Out 12 : Backlight for up-facing camera
' Out 13 : Down-facing illuminator

' Sw(8), Sw(9) : The complementary outputs of the optical sensor on the robot hand
' These should change state when the brass tube is raised by approximately 2.5 mm on
' the little black slide.  There is also a small red LED that indicates the state of the 
' optical sensor.

' Sw(10) : Vacuum switch.  	0 => Vac is OK 				1 => Vac is weak
' Sw(11) : Pressure switch.	0 => Pressure is not OK		1 => Pressure is OK



' *********************
' *** Code is below ***
' *********************

Function main

	Initialize

' Before using the system, make sure the taught points (7,15,36,37,38,39) are approximately on target.
' Then make sure chips are in all of these positions
' Also make sure Tray 1 Position 16 and 17 are open
' Then run the following 4 lines just one time to set up the system, and then comment them out for subsequent runs

' The following four lines are used to initially set up the relevant Points:
'ClearOffsets
'UpdatePickPoint(7)
'UpdatePickPoint(15)
'UpdateTrayPoints(1)

ShowOffsets

'S2T(7, 1, 17)
'S2T(15, 1, 18)


test2





'	S2T(7, 1, 16)
'	ShowOffsets

'	S2S(15, 7)
'	ShowOffsets
	
'	T2T(1, 16, 1, 17)
'	ShowOffsets

'	T2Tfast(1, 17, 1, 16)
'	ShowOffsets
	
'	T2S(1, 16, 15)
'	ShowOffsets
	
'	ScanDAT1
	
	
	FinishUp
	
Fend

Function test2
	
Integer m
Integer n
Integer k
String ImageName$

m = 0

For n = 1 To 1

For k = 1 To 15 Step 2
	
	T2S(1, k, 7)

	Tool 1
	Go P7 :U(-0.7)
	VSet chipview.Camera, 2
	VRun chipview
	ImageName$ = "c:\test\P7-" + Str$(m) + ".bmp"
	Print ImageName$
'	VSaveImage chipview, ImageName$
	Tool 0
	
	T2S(1, k + 1, 15)
	Tool 1
	Go P15 :U(-0.7)
	VSet chipview.Camera, 2
	VRun chipview
	ImageName$ = "c:\test\P15-" + Str$(m) + ".bmp"
	Print ImageName$
'	VSaveImage chipview, ImageName$
	Tool 0

	S2T(7, 1, k)
	S2T(15, 1, k + 1)
Print
m = m + 1
	
Next
	
Next

Fend


Function Initialize
	' Go to eHandler when an error occurs
	OnErr GoTo eHandler

	Robot 1
	Tool 0
	Reset
	Motor On
    MoveSpeed
	LoadPoints "robot1.pts"
	TLSet 1, XY(-55.8, 0, -25, 0)
	Pallet 1, P36, P37, P38, P39, 15, 6  'Note that these points are 20 mm above the PCS toggle
	Pallet 2, P40, P41, P42, P43, 15, 6  'Note that these points are 20 mm above the PCS toggle
	' For safety, Power is set to Low for the initial program
	' After program operation verification, Power can be set to High, using an abundance of caution
	Power High
	Arm 0
	Tool 0
	Jump P45 ' The PARK position
	Go Here -Z(32)
	Off 9
	Off 10
	Off 11
	On 12
	Exit Function

eHandler:
	' Error processing
	Print "Error", Err, " in function ", Erf$, ", line", Erl, ": ", ErrMsg$(Err)
	Quit All
Fend

Function MoveSpeed
	Speed 30
	Accel 10, 10
Fend

Function PickSpeed
	Speed 1
	Accel 1, 1
Fend

Function FinishUp
	Jump Park
	Off 10
	Off 9
	Off 11
	On 12
Fend

Function ScanTray1
	Integer m
	String ImageName$

	Tool 1
	Jump Pallet(1, 1) :U(179.3) /R ' Go to the first position in the pallet.  Tray 1: use "Righty" arm orientation for an easier life for the robot hand cables
	For m = 1 To 90
		Go Pallet(1, m) :U(179.3) /R 'use the tool offset defined in "initialize", at +180 rotation
		VSet chipview.Camera, 2
		VRun chipview
		ImageName$ = "c:\test\Img" + Str$(m) + ".bmp"
		Print ImageName$
		'VSaveImage chipview, ImageName$
	Next
	Tool 0
Fend

Function ScanDAT1

	String ImageName$
	
	Tool 1
	Jump P7 :U(-0.7)
	VSet chipview.Camera, 2
	VRun chipview
	ImageName$ = "c:\test\ImgDAT" + Str$(7) + ".bmp"
	Print ImageName$
	'VSaveImage chipview, ImageName$	

	Jump P15 :U(-0.7)
	VSet chipview.Camera, 2
	VRun chipview
	ImageName$ = "c:\test\ImgDAT" + Str$(15) + ".bmp"
	Print ImageName$
	'VSaveImage chipview, ImageName$	

	Tool 0
	
Fend


Function S2S(SocketA As Integer, SocketB As Integer)
' This function moves a chip from SocketA to SocketB	
'First check that g_OffsetOK is TRUE for the destination, otherwise no action, just warning message
If g_OffsetOK(SocketB) = True Then
	'Print "Initial g_Offsets:", g_Offset(SocketA, 0), g_Offset(SocketA, 1), g_Offset(SocketA, 2), g_Offset(SocketA, 3)
	Jump P(SocketA) - XY(g_Offset(SocketA, 0), g_Offset(SocketA, 1), 0, g_Offset(SocketA, 3)) LimZ -30  '**Don't use the Zpcs**
	'Print "At Pick:", Here
	SocketPick
	FindChip4
	If FindStat = 1 Then
		Print "FindChip: Xoff, Yoff, Uoff:", Xoff, Yoff, Uoff
		'Update the offsets for the pick point
		g_Offset(SocketA, 0) = g_Offset(SocketA, 0) + Xoff	'Update the X offset
		g_Offset(SocketA, 1) = g_Offset(SocketA, 1) + Yoff	'Update the Y offset
		g_Offset(SocketA, 2) = CZ(P(SocketA)) - Zpcs - 20 	'Not used for motion.  Should be close to zero.
		g_Offset(SocketA, 3) = g_Offset(SocketA, 3) + Uoff	'Update the U offset
		'Print "New Point", SocketA, " g_Offsets:", g_Offset(SocketA, 0), g_Offset(SocketA, 1), g_Offset(SocketA, 2), g_Offset(SocketA, 3)
		' Go to the destination, applying the prior knowledge of the offsets
		Jump P(SocketB) - XY(g_Offset(SocketB, 0), g_Offset(SocketB, 1), 0, g_Offset(SocketB, 3)) LimZ -30
		' Then apply the offsets for the chip currently on the robot hand
		TGo XY(Xoff, Yoff, 0, Uoff)
		SocketPlace
	Else
		Print "***FindChip failed"
		' Return ASIC to destination?
	EndIf

Else
	Print "***S2S: cannot deliver chip to Point:", SocketB, " until g_OffsetOK is TRUE. Run UpdatePickPoint"
	' Set an error code?
EndIf

Fend

Function S2T(SocketA As Integer, TrayB As Integer, TrayPosB As Integer)
' This function moves a chip from SocketA to TrayB TrayPosB
	'First go to the pick point, using the previously stored offsets to that position
	Jump P(SocketA) - XY(g_Offset(SocketA, 0), g_Offset(SocketA, 1), 0, g_Offset(SocketA, 3)) LimZ -30  '**Don't use the Zpcs**
	SocketPick
	FindChip4
	If FindStat = 1 Then
		Print "FindChip: Xoff, Yoff, Uoff:", Xoff, Yoff, Uoff
		'Update the offsets for the pick point
		g_Offset(SocketA, 0) = g_Offset(SocketA, 0) + Xoff	'Update the X offset
		g_Offset(SocketA, 1) = g_Offset(SocketA, 1) + Yoff	'Update the Y offset
		g_Offset(SocketA, 2) = CZ(P(SocketA)) - Zpcs - 20 	'Not used for motion.  Monitor; should be stable and close to zero.
		g_Offset(SocketA, 3) = g_Offset(SocketA, 3) + Uoff	'Update the U offset
		Jump Pallet(TrayB, TrayPosB) LimZ -30
		' Then apply the offsets for the chip currently on the robot hand
		TGo XY(Xoff, Yoff, 0, Uoff)
		TrayPlace
	Else
		Print "***FindChip failed"
		' Return ASIC to destination?
	EndIf
Fend
	
Function T2S(TrayA As Integer, TrayPosA As Integer, SocketB As Integer)
'This function moves a chip from TrayA,TrayPosA to SocketB
'First check that g_OffsetOK is TRUE for the destination, otherwise no action, just warning message
If g_OffsetOK(SocketB) = True Then
	Jump Pallet(TrayA, TrayPosA) LimZ -30
	TrayPick
	FindChip4
	If FindStat = 1 Then
		Print "FindChip: Xoff, Yoff, Uoff:", Xoff, Yoff, Uoff
		' Go to the destination, applying the prior knowledge of the offsets
		Jump P(SocketB) - XY(g_Offset(SocketB, 0), g_Offset(SocketB, 1), 0, g_Offset(SocketB, 3)) LimZ -30
		' Then apply the offsets for the chip currently on the robot hand
		TGo XY(Xoff, Yoff, 0, Uoff)
		SocketPlace
	Else
		Print "***FindChip failed"
		' Return ASIC to destination?
	EndIf

Else
	Print "***S2S: cannot deliver chip to Point:", SocketB, " until g_OffsetOK is TRUE. Run UpdatePickPoint"
	' Set an error code?
EndIf
	
	
Fend

Function T2T(TrayA As Integer, TrayPosA As Integer, TrayB As Integer, TrayPosB As Integer)
'This function moves a chip from TrayA,TrayPosA to TrayB,TrayPosB

	Jump Pallet(TrayA, TrayPosA) LimZ -30
	TrayPick
	FindChip4
	If FindStat = 1 Then
		Print "FindChip: Xoff, Yoff, Uoff:", Xoff, Yoff, Uoff
		Jump Pallet(TrayB, TrayPosB) LimZ -30
		' Then apply the offsets for the chip currently on the robot hand
		TGo XY(Xoff, Yoff, 0, Uoff)
		TrayPlace
	Else
		Print "***FindChip failed"
		' Return ASIC to destination?
	EndIf
	
Fend

Function T2Tfast(TrayA As Integer, TrayPosA As Integer, TrayB As Integer, TrayPosB As Integer)
	Jump Pallet(TrayA, TrayPosA) LimZ -30
	TrayPick
	Jump Pallet(TrayB, TrayPosB) LimZ -30
	TrayPlace
Fend

Function TrayPick
	' This motion sequence is designed to start from a position 20 mm above
	' the point where the PCS sensor is expected to toggle
	
	' The robot will move at most 23 mm down, and stop when the PCS toggles

	' If there is an ASIC, the Zrel should be close to -20 mm, and the contact with 
	' the ASIC should have raised the brass tube up by about 2.5 mm on its slide mechanism	
	
	' If there is not as ASIC, the Zrel should be -22 mm to -23 mm, depending on the 
	' socket/tray structure below the empty ASIC position

	PickSpeed
	Zset_SP = CZ(Here)								'Save the starting Z position
	On 12  		'backlight
	On 10  		'vacuum valve opened to ensure pump starts without load
	On 9   		'Turn on the vacuum pump
	Off 11 		'Make sure the pushers are not engaged
	Off 10 		'Turn off the vacuum valve
	Go Here -Z(23) Till Sw(8) = On Or Sw(9) = Off	'Redundant lines from the PCS are used for safety
	Zpcs = CZ(Here)									'Robot stops at PCS, so use the current position
	Zrel = Zpcs - Zset_SP
	Zfound = TillOn
	On 10											'Open the vacuum valve 
	Wait 1
	Go Here :Z(Zset_SP)								'Finish back where the pick operation started
	MoveSpeed
Fend

Function TrayPlace
	' This motion sequence is designed to start from a position 20 mm above
	' the point where the PCS sensor is expected to toggle
	
	' The robot will move at most 23 mm down, and stop when the PCS toggles
	' If the tray is in place, and the tray position is empty, the Zrel should be 
	' close to -20 mm
	
	' If Zrel is closer to -18.5, then it appears that an ASIC is already in that Tray position
	' If Zrel is -23 mm, then the ASIC in hand did not land in a tray position
	
	' It is assumed that the vacuum pump is on and the vacuum valve is open when entering this function
	
	Zset_SP = CZ(Here)								'Save the starting Z position
	Off 11  										'Make sure that the pushers are not engaged
    PickSpeed
	Go Here -Z(23) Till Sw(8) = On Or Sw(9) = Off	'Redundant lines from the PCS are used for safety
	Zpcs = CZ(Here)									'Robot stops at PCS, so use the current position
	Zrel = Zpcs - Zset_SP
	Zfound = TillOn
	Off 10  'Turn off the vacuum valve to release the ASIC
	Wait 1
	Go Here :Z(Zset_SP)								'Finish back where the pick operation started
	'Off 9  'Optionally turn off the vacuum pump for now
	MoveSpeed
Fend

Function SocketPick
	' This motion sequence is designed to start from a position 20 mm above
	' the point where the PCS (payload contact sensor) switches state
	
	' The PCS switch point is about 2.5 mm below the point where the brass tube touches the ASIC

	' The motion will go go down by at most 23 mm
	
	' At this point the clearance between EOAT and Socket is ~ xx.x mm

	Zset_SP = CZ(Here)
	PickSpeed
	On 12  'backlight on in general
	On 10  'vacuum valve opened to ensure pump starts without load
	On 9   'Turn on the vacuum pump
	On 11 	'Turn on the pushers
	Off 10 ' Turn off the vacuum valve

	Go Here -Z(23) Till Sw(8) = On Or Sw(9) = Off	'Redundant lines from the PCS are used for safety
	Zpcs = CZ(Here)									'Robot stops at PCS, so use the current position
	Zrel = Zpcs - Zset_SP
	Zfound = TillOn
	On 10 'Open the vacuum valve 
	Wait 1

	Go Here :Z(Zset_SP)								'Finish back where the pick operation started

	Off 11 'Retract the pushers
	
	MoveSpeed

Fend

Function SocketPlace
	Zset_SP = CZ(Here)
	PickSpeed
	On 11 	'Turn on the pushers

	Go Here -Z(23) Till Sw(8) = On Or Sw(9) = Off	'Redundant lines from the PCS are used for safety
	Zpcs = CZ(Here)									'Robot stops at PCS, so use the current position
	Zrel = Zpcs - Zset_SP
	Zfound = TillOn

	Off 10 'Turn off vacuum and vent the vacuum pick plumbing
	Off 9 ' Also turn off the vacuum pump
	Wait 1

	Go Here :Z(Zset_SP)								'Finish back where the pick operation started

	Off 11 'Retract the pushers
	
	MoveSpeed
	
Fend


Function UpdatePickPoint(WP As Integer)
' This will fully update one pick point	
' Global integer WP ("Working Point") needs to be set before entering this function
' After the pick point is updated, the g_Offset values for that point are set to zero
' Also the g_OffsetOK boolean is set to TRUE for this point, since we can now deliver ASICs
' In other words: if we have never observed the Socket position, it is not safe to deliver a chip to that socket
' This function finishes by returning the chip to its original location

' To use this function:
' (1) Manually install a chip in the socket 
' (2) Position the robot such that the brass tube that picks up the chip is roughly centered on the chip
' (3) Position the robot such that the end of the tube is approximately 20 mm above the point where it touches the chip. 
' (4) Set the variable WP to the "Working Point" that you are working on.
' (5) Call the function (read the function comments for details on operation)

Print "UpdatePickPoint starting for point:", WP
If WP < 44 Then 			' It's a valid pick point for Update
	
	P(WP) = P(WP) :U(0)		' Work at zero degrees rotation of the robot hand

	Jump P(WP) LimZ -30		' Jump to the starting point
	
	Print "Point", WP, " initial Z coordinate", CZ(P(WP))

	Off 12 											'Turn off the backlight for a visual indication
	PickSpeed										'Move Slowly
	Go Here -Z(30) Till Sw(8) = On Or Sw(9) = Off	'Move down until PCS trip or too far
	On 12 											'Turn the backlight back on for a visual indication
	Zpcs = CZ(Here)									'Robot stops at PCS, so use the current stopped position
	Zrel = Zpcs - CZ(P(WP))							'Relative movement is stored in Zrel
	Zfound = TillOn									'If Till condition is on, then we had a PCS event, otherwise traveled to limit

	If Zfound Then
		Print "PCS contact at ", Zpcs
		P(WP) = P(WP) :U(0) :Z(Zpcs + 20)  		 	'reset the Z position of the point to 20 mm above the PCS toggle point
		Print "Point", WP, " updated Z coordinate", CZ(P(WP))
	Else
		Print "***PCS did not toggle, Z coordinate not updated"
		' Something did not gio as expected, set a flag and take action
	EndIf
	
	Go P(WP)										' Go to the updated pick position

	If WP < 36 Then
		SocketPick
	ElseIf WP < 44 Then
		TrayPick
	EndIf
	
	Print "Finding the offsets of the chip in hand..."
	FindChip4
	
	Jump P(WP) LimZ -30

	If WP < 36 Then
		SocketPlace
	ElseIf WP < 44 Then
		TrayPlace
	EndIf

	Print "Xoff,Yoff,Uoff: ", Xoff, Yoff, Uoff
	If FindStat = 1 Then
		Print "Old Point ", WP, ": ", P(WP)
		P(WP) = P(WP) - XY(Xoff, Yoff, 0, Uoff)
		Print "New Point ", WP, ": ", P(WP)
		If WP < 36 Then
			Print "Also setting g_Offset values for point:", WP, " to zero"
			g_Offset(WP, 0) = 0
			g_Offset(WP, 1) = 0
			g_Offset(WP, 2) = 0
			g_Offset(WP, 3) = 0
			Print "Also setting g_OffsetOK boolean for point:", WP, " to TRUE"
			g_OffsetOK(WP) = True
		EndIf
		
		Print "Saving robot1.pts"
		SavePoints "robot1.pts"

	Else
		Print "***UpdatePickPoint: Failed to find the Chip, not updating Point:", WP
		Print "***UpdatePickPoint: Also not clearing the g_Offset values for Point:", WP
		Print "***Not trusting Offset values, setting g_OffsetOK to false:"
		g_OffsetOK(WP) = False
	EndIf

Else
	Print "***UpdatePickPoint: WP is > 43, cannot update"
EndIf

Fend

Function FindChip4
' Point P44 is assumed to be near the center of the upfacing camera
' Precise positioning of P44 is not important- we are making relative measurements
	
' This function will move two opposite corners of the ASIC over the camera and take 
' measurements, and then rotate 180 degrees and take those measurements again

' Note that the image distortions are expected to be minimal near the center
' of the camera image.  Far from center there is increasing parallax.
	
' The CameraViewQ1 and CameraViewQ3 functions return "Correlation" measurements
' based on fitting the image to a model of the corner of the ASIC.

' In the case of a failure to detect the ASIC corners, FindStat will be 
' set to zero and the calling function should handle this error

' FindStat = 0 indicates a failure to find the ASIC corners in at least one imaging operation
' FindStat = 1 indicates apparent success at finding the ASIC corners
' FindStat = -1 indicates that the computed ASIC offsets are out of range and have been set to zero

' Map of the coordinates determined from the image recognition:
'
' When the ASIC is held over camera with U = +90 degrees:
'       X3
'   Y3  -------
'      |       |
'      |       |
'      |       |
'       -------  Y1
'             X1
'
' When the ASIC is held over camera with U = -90 degrees:
'       X1P
'  Y1P  -------
'      |       |
'      |       |
'      |       |
'       -------  Y3P
'             X3P
'
' From these measurements we can both find the center of the ASIC and 
' find the center of rotation, which is the J3 axis of the SCARA robot

' Note that the J3 axis and the ASIC center are generally different.
' Also note that the robot U coordinate and the ASIC orientation are generally different.
' These differences can be described by an offset and rotation in a coordinate
' system that travels with the robot hand.  

' Also, when picking from a location, the tool coordinates of the picked
' chip can inform the placement of the next chip into that position. 

	' Coordinates extracted from the measurements, and quality scores
	Real X1, Y1, X3, Y3        'ASIC fiducials in real world coordinates, with chip rotated +90 degrees
	Real X1P, Y1P, X3P, Y3P    'ASIC fiducials in real world coordinates, with chip rotated -90 degrees
	Real Q1Score, Q3score, Q1Pscore, Q3Pscore  'The quality scores of the correlation fits.
		
	' Values calculated from the measurements:
	Real Xcenter, Ycenter		'Observed center of ASIC when rotated +90 degrees
	Real XcenterP, YcenterP		'Observed center of ASIC when rotated -90 degrees
	Real J3X, J3Y				'Average of the above to ASIC center measurements- should coincide with J3 axis position
	
	Real Angle					'Angle of the ASIC derived from the +90 degree rotation
	Real AngleP					'Angle of the ASIC derived from the -90 degree rotation
	Real AngleAVG				'Average of above

	Real cXoff, cYoff, cUoff	'These are the computed offsets from J3 (robot) center to ASIC center
	
	Integer MoveOut 		 	'How far to move out from the P4 position (this prevents pincushion effect on off-axis pins)
	MoveOut = 7     		 	' 7 mm for the 128 pin ASICs
	
	CorrFailCount = 0			'Reset the counter of failures to find the ASIC corners via Correlation	
	
	Jump P44 +X(MoveOut) +Y(MoveOut) :U(90) LimZ -30  ' to get X1 and Y1
	CameraViewQ1									' But can be used as a test of the offsets
	X1 = Xcorr - MoveOut							' Correct for the fact that we are off-center
	Y1 = Ycorr - MoveOut							' Correct for the fact that we are off-center
	Q1score = Qscore
	
	Go P44 -X(MoveOut) -Y(MoveOut) :U(90) ' to get X3 and Y3
	CameraViewQ3
	X3 = Xcorr + MoveOut
	Y3 = Ycorr + MoveOut
	Q3score = Qscore

	Go P44 -X(MoveOut) -Y(MoveOut) :U(-90) 'To get X3P and Y3P
	CameraViewQ3
	X1P = Xcorr + MoveOut
	Y1P = Ycorr + MoveOut
	Q3Pscore = Qscore

	Go P44 +X(MoveOut) +Y(MoveOut) :U(-90) 'To get X1P and Y1P
	CameraViewQ1
	X3P = Xcorr - MoveOut
	Y3P = Ycorr - MoveOut
	Q1Pscore = Qscore

'Calculate the ASIC center coordinates from the fiducials:
Xcenter = (X1 + X3) /2 		' in the +90 orientation
Ycenter = (Y1 + Y3) /2 		' in the +90 orientation
XcenterP = (X1P + X3P) /2 	' in the -90 orientation
YcenterP = (Y1P + Y3P) /2 	' in the -90 orientation

'Print "ASIC center at +90 degrees is: ", Xcenter, " , ", Ycenter
'Print "ASIC center at -90 degrees is: ", XcenterP, " , ", YcenterP

' Now find the center of robot J3
J3X = (Xcenter + XcenterP) /2
J3Y = (Ycenter + YcenterP) /2
'Print "J3 axis  found at: ", J3X, " , ", J3Y

' Now figure out the angle of the ASIC relative to the robot cartesian axes:
Angle = 45 - RadToDeg(Atan2(X3 - X1, Y3 - Y1))
AngleP = 45 - RadToDeg(Atan2(X1P - X3P, Y1P - Y3P))
AngleAVG = (Angle + AngleP) /2
'Print "Angle from +90 observations= ", Angle
'Print "Angle from -90 observations= ", AngleP
'Print "Average Angle= ", AngleAVG

' Now find the vector from ASIC center to J3
cXoff = J3X - Xcenter
cYoff = J3Y - Ycenter
cUoff = AngleAVG

' The vector was determined with J4 at 90 degrees, so
' also perform a 90 degree coordinate rotation to put 
' the results into the robot's world coordinate frame
Xoff = cYoff
Yoff = -cXoff
Uoff = cUoff
'Print "The J3 - ASIC center offset in world coordinates is: ", Xoff, Yoff
' And note that when Robot U is at zero degrees, the world and tool coordinate
' systems are rotationally aligned.  Thus this is also the Tool offset in World Coordinates.

' Now set the FindStat status and zero the offsets in case of errors
If (CorrFailCount = 0) Then
	FindStat = 1
	Else
		Xoff = 0
		Yoff = 0
		Uoff = 0
		FindStat = 0
	EndIf
	
'Finally, limit these offest values for safety (For now: limit to +/-3 mm and +/- 3 degrees
If (Xoff > 3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
If (Xoff < -3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
If (Yoff > 3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
If (Yoff < -3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
If (Uoff > 3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
If (Uoff < -3) Then
	Xoff = 0
	Yoff = 0
	Uoff = 0
	FindStat = -1
	EndIf
Fend

Function CameraViewQ1
	Boolean found0
	Real CorrX, CorrY, CorrU

	VSet Fiducial128Q1X.Camera, 1
	VSet Fiducial128Q1X.Calibration, "StingerCalibrate"
	VRun Fiducial128Q1X
	VGet Fiducial128Q1X.Corr01.RobotXYU, found0, CorrX, CorrY, CorrU
	VGet Fiducial128Q1X.Corr01.Score, Qscore
	
	If found0 Then
		Xcorr = CorrX
		Ycorr = CorrY
	Else
		Print "*** CameraViewQ1 - failed!"
		CorrFailCount = CorrFailCount + 1
	EndIf
	
Fend

Function CameraViewQ3
	Boolean found0
	Real CorrX, CorrY, CorrU

	VSet Fiducial128Q3X.Camera, 1
	VSet Fiducial128Q3X.Calibration, "StingerCalibrate"
	VRun Fiducial128Q3X
	VGet Fiducial128Q3X.Corr01.RobotXYU, found0, CorrX, CorrY, CorrU
	VGet Fiducial128Q1X.Corr01.Score, Qscore

	If found0 Then
		Xcorr = CorrX
		Ycorr = CorrY
	Else
		Print "*** CameraViewQ3X - failed!"
		CorrFailCount = CorrFailCount + 1
	EndIf

Fend

Function UpdateTrayPoints(Tray As Integer)
	
' For each corner of Tray, this will update the pick points for the four corners
' Then redefine the Tray
' Then save the Points file

' Failure modes: not yet addressed

If Tray = 1 Then
	Print "Updating Tray 1: A 90 position Tray in the LEFT position"
	UpdatePickPoint(36)
	UpdatePickPoint(37)
	UpdatePickPoint(38)
	UpdatePickPoint(39)
	Print "Redefining Pallet ", Tray
	PalletClr Tray
	Pallet Tray, P(36), P(37), P(38), P(39), 15, 6
	EndIf
	
If Tray = 2 Then
	Print "Updating Tray 2: A 90 position Tray in the RIGHT position"
	UpdatePickPoint(40)
	UpdatePickPoint(41)
	UpdatePickPoint(42)
	UpdatePickPoint(43)
	PalletClr Tray
	Pallet Tray, P(40), P(41), P(42), P(43), 15, 6
	EndIf
	
' Now save the points:
	Print "Saving robot1.pts"
	SavePoints "robot1.pts"

Fend

Function ClearOffsets
	' Utility function for clearing out the g_Offset values and the g_OffsetOK flags
	Integer k
	Print "Clearing the contents of the g_Offset array and setting all g_OffsetOK flags to FALSE"
	For k = 0 To 35
		g_OffsetOK(k) = False
		g_Offset(k, 0) = 0
		g_Offset(k, 1) = 0
		g_Offset(k, 2) = 0
		g_Offset(k, 3) = 0
	Next k
Fend

Function CheckOffsets
' Check that the array of socket offsets "g_Offset"	is within limits
Real X_OK, Y_OK, U_OK
X_OK = 1.5		'For now, pick these at 1.5 mm
Y_OK = 1.5
U_OK = 1.5		'For now, pick this at 1.5 degrees

Integer k
Integer CheckErrCnt
CheckErrCnt = 0

For k = 0 To 35
	If g_OffsetOK(k) = True Then
		If g_Offset(k, 0) > X_OK Then CheckErrCnt = CheckErrCnt + 1
		If g_Offset(k, 1) > Y_OK Then CheckErrCnt = CheckErrCnt + 1
		If g_Offset(k, 3) > U_OK Then CheckErrCnt = CheckErrCnt + 1
		If g_Offset(k, 0) < -X_OK Then CheckErrCnt = CheckErrCnt + 1
		If g_Offset(k, 1) < -Y_OK Then CheckErrCnt = CheckErrCnt + 1
		If g_Offset(k, 3) < -U_OK Then CheckErrCnt = CheckErrCnt + 1
	EndIf
	
Next

If CheckErrCnt = 0 Then
	Print "CheckOffsets: x,y,u are OK"
Else
	Print "***CheckOffsets failed: ", CheckErrCnt, " Offsets are out of range, please manually fix this"
EndIf

Fend

Function ShowOffsets
Integer k
'Print "Showing only the defined g_Offset values:"
For k = 0 To 35
	If g_OffsetOK(k) = True Then
		Print "k:", k, " Offsets:", g_Offset(k, 0), g_Offset(k, 1), g_Offset(k, 2), g_Offset(k, 3)
	EndIf
Next
Print

Fend
Function CreateDAT1
' Given P(0), create estimates for the other 17 points of DAT1	
' Note that this can be written for building up from any known point

' Based on the DAT board geometry
' Entry condition: P(0) is taught by the operator
' Taught point should be "standard" 20 mm above the point where the PCS toggles with an ASIC in the socket

' Setup: Place an ASIC in socket position zero
' Manually position the robot over this socket
' Try to get the brass tube centered on the ASIC
' Try to get the robot z-position adjusted to where the brass tube is just touching the ASIC

Integer k

'First test P(0), ensure that it is basically reasonable
'P(0) = XY(82.55, 273.05, 90, 0)
' Now that P(0) is validated, generate the remaining points:

P(1) = XY(CX(P0), CY(P0) - 76.2, CZ(P0), 0) /L
P(2) = XY(CX(P0), CY(P0) - 152.4, CZ(P0), 0) /L
P(3) = XY(CX(P0), CY(P0) - 228.6, CZ(P0), 0) /L
P(4) = XY(CX(P0), CY(P0) - 317.5, CZ(P0), 0) /L
P(5) = XY(CX(P0), CY(P0) - 393.7, CZ(P0), 0) /L
P(6) = XY(CX(P0), CY(P0) - 469.9, CZ(P0), 0) /L
P(7) = XY(CX(P0), CY(P0) - 546.1, CZ(P0), 0) /L

P(8) = XY(CX(P0) + 82.55, CY(P0), CZ(P0), 0) /L
P(9) = XY(CX(P0) + 82.55, CY(P0) - 76.2, CZ(P0), 0) /L
P(10) = XY(CX(P0) + 82.55, CY(P0) - 152.4, CZ(P0), 0) /L
P(11) = XY(CX(P0) + 82.55, CY(P0) - 228.6, CZ(P0), 0) /L
P(12) = XY(CX(P0) + 82.55, CY(P0) - 317.5, CZ(P0), 0) /L
P(13) = XY(CX(P0) + 82.55, CY(P0) - 393.7, CZ(P0), 0) /L
P(14) = XY(CX(P0) + 82.55, CY(P0) - 469.9, CZ(P0), 0) /L
P(15) = XY(CX(P0) + 82.55, CY(P0) - 546.1, CZ(P0), 0) /L

P(16) = XY(CX(P0) + 184.15, CY(P0) - 114.3, CZ(P0), 0) /L
P(17) = XY(CX(P0) + 184.15, CY(P0) - 431.8, CZ(P0), 0) /L

For k = 0 To 17
	Print "P", k, P(k)
Next
	
Print "Saving robot1.pts"
SavePoints "robot1.pts"
	
Fend

