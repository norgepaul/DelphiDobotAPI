unit Dobot.Dll;

interface

uses
  SysUtils,

  Dobot.DLL.Types;

function DobotExec(): Integer; cdecl; external 'DobotDll.dll';
function SearchDobot(dobotNameList: PChar; maxLen: LongWord): Integer; cdecl; external 'DobotDll.dll';
function ConnectDobot(portname: PChar; baudrate: LongWord): Integer; cdecl; external 'DobotDll.dll';
procedure DisconnectDobot(); cdecl; external 'DobotDll.dll';
function SetCmdTimeout(cmdTimeout: LongWord): Integer; cdecl; external 'DobotDll.dll';
function SetDeviceSN(deviceSN: PChar): Integer; cdecl; external 'DobotDll.dll';
function GetDeviceSN(deviceSN: PChar; maxLen: LongWord): Integer; cdecl; external 'DobotDll.dll';
function SetDeviceName(deviceName: PChar): Integer; cdecl; external 'DobotDll.dll';
function GetDeviceName(deviceName: PChar; maxLen: LongWord): Integer; cdecl; external 'DobotDll.dll';
function GetDeviceVersion(majorVersion: PByte; minorVersion: PByte; revision: PByte): Integer; cdecl; external 'DobotDll.dll';

// Pose and Kinematics parameters are automatically get
function GetPose(pose: PPose): Integer; cdecl; external 'DobotDll.dll';
function GetPoseEx(pose: PPose): Integer; cdecl; external 'DobotDll.dll';
function ResetPose(manual: LongBool; rearArmAngle: single; fronArmAngle: single): Integer; cdecl; external 'DobotDll.dll';
function GetKinematics(kinematics: PKinematics): Integer; cdecl; external 'DobotDll.dll';

// Alarms
function GetAlarmsState(alarmState: PByte; len: PLongWord; maxLen: LongWord): Integer; cdecl; external 'DobotDll.dll';
function ClearAllAlarmsState(): Integer; cdecl; external 'DobotDll.dll';

// HOME
function SetHOMEParams(homeParams: PHomeParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetHOMEParams(homeParams: PHomeParams): Integer; cdecl; external 'DobotDll.dll';
function SetHOMECmd(homeCmd: PHomeCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// EndEffector
function SetEndEffectorParams(endEffectorParams: PEndEffectorParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetEndEffectorParams(endEffectorParams: PEndEffectorParams): Integer; cdecl; external 'DobotDll.dll';
function SetEndEffectorLaser(enableCtrl: LongBool; laserOn: LongBool; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetEndEffectorLaser(var isCtrlEnabled: LongBool; var isOn: LongBool): Integer; cdecl; external 'DobotDll.dll';
function SetEndEffectorSuctionCup(enableCtrl: LongBool; suck: LongBool; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetEndEffectorSuctionCup(var isCtrlEnabled: LongBool; var isSucked: LongBool): Integer; cdecl; external 'DobotDll.dll';
function SetEndEffectorGripper(enableCtrl: LongBool; grip: LongBool; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetEndEffectorGripper(var isCtrlEnabled: LongBool; var isGripped: LongBool): Integer; cdecl; external 'DobotDll.dll';

// JOG functions
function SetJOGJointParams(jointJogParams: PJOGJointParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetJogJointParams(jointJogParams: PJOGJointParams): Integer; cdecl; external 'DobotDll.dll';
function SetJOGCoordinateParams(coordinateJogParams: PJOGCoordinateParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetJOGCoordinateParams(coordinateJogParams: PJOGCoordinateParams): Integer; cdecl; external 'DobotDll.dll';
function SetJOGCommonParams(jogCommonParams: PJOGCommonParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetJOGCommonParams(jogCommonParams: PJOGCommonParams): Integer; cdecl; external 'DobotDll.dll';
function SetJOGCmd(jogCmd: PJogCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// PTP functions
function SetPTPJointParams(ptpJointParams: PPTPJointParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetPTPJointParams(ptpJointParams: PPTPJointParams): Integer; cdecl; external 'DobotDll.dll';
function SetPTPCoordinateParams(ptpCoordinateParams: PPTPCoordinateParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetPTPCoordinateParams(ptpCoordinateParams: PPTPCoordinateParams): Integer; cdecl; external 'DobotDll.dll';
function SetPTPJumpParams(ptpJumpParams: PPTPJumpParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetPTPJumpParams(ptpJumpParams: PPTPJumpParams): Integer; cdecl; external 'DobotDll.dll';
function SetPTPCommonParams(ptpCommonParams: PPTPCommonParams;isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetPTPCommonParams(ptpCommonParams: PPTPCommonParams): Integer; cdecl; external 'DobotDll.dll';
function SetPTPCmd(ptpCmd: PPTPCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// CP functions
function SetCPParams(cpParams: PCPParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetCPParams(cpParams: PCPParams): Integer; cdecl; external 'DobotDll.dll';
function SetCPCmd(cpCmd: PCPCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function SetCPLECmd(cpCmd: PCPCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// ARC
function SetARCParams(arcParams: PARCParams; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';
function GetARCParams(arcParams: PARCParams): Integer; cdecl; external 'DobotDll.dll';
function SetARCCmd(arcCmd: PARCCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// WAIT
function SetWAITCmd(waitCmd: PWAITCmd; isQueued: LongBool; queuedCmdIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

// Queued command
function SetQueuedCmdStartExec(): Integer; cdecl; external 'DobotDll.dll';
function SetQueuedCmdStopExec(): Integer; cdecl; external 'DobotDll.dll';
function SetQueuedCmdForceStopExec(): Integer; cdecl; external 'DobotDll.dll';
function SetQueuedCmdStartDownload(totalLoop: LongWord; lineperloop: LongWord): Integer; cdecl; external 'DobotDll.dll';
function SetQueuedCmdStopDownload(): Integer; cdecl; external 'DobotDll.dll';
function SetQueuedCmdClear(): Integer; cdecl; external 'DobotDll.dll';
function GetQueuedCmdCurrentIndex(queuedCmdCurrentIndex: Puint64): Integer; cdecl; external 'DobotDll.dll';

implementation

end.
