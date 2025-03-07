'#define REPO_DIR "C:\EpsonRC70\Projects\DUNE\CE-RTS"
'#define RTS_DATA "C:\Users\coldelec\RTS\"
#define NTRAYS 2
#define TRAY_NCOLS 15 ' For TRAY and DAT arays use maximum size
#define TRAY_NROWS 6
#define TRAY_NCOLS_S 15
#define TRAY_NROWS_S 6
#define TRAY_NCOLS_L 10
#define TRAY_NROWS_L 4
Global Int32 trayNCols ' Set to correct chip layout for loops
Global Int32 trayNRows

#define N_LARASIC_SOC 8
#define N_COLDADC_SOC 8
#define N_COLDATA_SOC 2
#define N_MSU_TEST_SOC 8
#define NSOCKETS 18
Global Int32 nSoc ' Set by chip type for testing

#define CONTACT_DIST 10.0

Global String REPO_DIR$
Global String PROJ_DIR$
Global String RTS_DATA$

Global String SITE$
Global String CHIPTYPE$
Global String POINTS_FILE$
Global Double HAND_U0
Global Double DF_CAM_X_OFF_U0
Global Double DF_CAM_Y_OFF_U0
Global Double DF_CAM_Z_OFF
Global Double DF_CAM_FOCUS

' corrections for the trays
Global Double tray_X(NTRAYS, TRAY_NCOLS, TRAY_NROWS)
Global Double tray_Y(NTRAYS, TRAY_NCOLS, TRAY_NROWS)
Global Double tray_U(NTRAYS, TRAY_NCOLS, TRAY_NROWS)

' corrections for the sockets
Global Double DAT_X(2, NSOCKETS)
Global Double DAT_Y(2, NSOCKETS)
Global Double DAT_U(2, NSOCKETS)
'
'' chip SN
''Global String chip_SN$

