unit Dobot.Controller;

interface

uses
  System.SysUtils, System.Classes,

  Dobot.Classes,
  Dobot.Dll.Types,
  Dobot.Dll,
  Dobot.Types;

type
  TOnLog = procedure(Sender: TObject; const LogText: String; const LogLevel: TDobotLogLevel) of object;

  TBaseDobotController = class(TComponent)
  private
    FOnLog: TOnLog;
    FNextTimerTicks: Cardinal;
    FConnected: Boolean;
    FLastMoveTicks: Cardinal;
    FMoveTickInterval: Cardinal;
    FLastPose: TPose;
    FAxisMovements: TDobotAxisMovements;

    procedure CheckTimer;
  protected
    FCmdIndex: UInt64;
    FMoveCmdIndex: UInt64;

    procedure NextCommandIndex;
    procedure CheckResult(const Value: Integer);
    procedure CheckBusy;

    procedure DoLog(const LogText: String; const LogLevel: TDobotLogLevel = TDobotLogLevel.Info); virtual;
    procedure DoOnTimer; virtual;
    procedure DoLoop; virtual;
    procedure DoBusy(out Value: Boolean); virtual;

    // --- Connection
    procedure DoConnect(const SerialPort: String = ''; const BaudRate: Integer = 115200; const Timeout: Integer = 3000); virtual;
    procedure DoDisconnect; virtual;
    function DoConnected: Boolean; virtual;

    // --- Misc
    procedure DoExec; virtual;

    // --- Settings
    procedure DoSetCoordinateJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True); virtual;
    procedure DoSetJointJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True); virtual;
    procedure DoSetPTPJointParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True); virtual;
    procedure DoSetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean = True); virtual;
    procedure DoSetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean = True); virtual;
    procedure DoSetPTPJumpParameters(const jumpHeight, zLimit: Single; const Queued: Boolean = True); virtual;
    procedure DoSetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single; const Queued: Boolean = True); virtual;
    procedure DoSetCommandTimeout(const Value: Integer); virtual;
    procedure DoSetQueuedCmdClear; virtual;
    procedure DoSetQueuedCmdStartExec; virtual;

    // --- States
    procedure DoGetDeviceVersion(out Major, Minor, Rev: Byte); virtual;
    procedure DoGetPose(out Pose: TPose); virtual;
    procedure DoGetAlarmState(out AlarmState: TArray<Byte>); virtual;
    function DoGetAlarms: TDobotAlarms; virtual;
    function DoGetCurrentCommandIndex: UInt64; virtual;

    // --- Actions
    procedure DoHome(const Queued: Boolean = True); virtual;
    procedure DoStop(const Force: Boolean); virtual;

    // --- Move
    procedure DoMove(const Command: TDobotJogCommand; const MoveType: TDobotMoveType; const MoveForMS: Cardinal = 100; const Queued: Boolean = True); virtual;
    function DoMoving: Boolean; virtual;
    function DoGetAxisMovements: TDobotAxisMovements; virtual;
    function DoLastMoveTicks: Cardinal; virtual;

    // --- Point to Point
    procedure DoSetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single; const Queued: Boolean = True); virtual;

    // --- EndExtenders
    procedure DoSetGripperPosition(const GripperPosition: TGripperPosition; const Queued: Boolean = True); virtual;

    property OnLog: TOnLog read FOnLog write FOnLog;
    property MoveTickInterval: Cardinal read FMoveTickInterval write FMoveTickInterval;
  public
    // --- Control
    procedure Loop; // Call every 100ms

    function Busy: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TDobotController = class(TBaseDobotController)
  public
    // --- Connection
    procedure Connect(const SerialPort: String = ''; const BaudRate: Integer = 115200; const Timeout: Integer = 3000);
    procedure Disconnect;
    function Connected: Boolean;

    // --- Misc
    procedure Exec;

    // --- Settings
    procedure SetCoordinateJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True);
    procedure SetJointJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True);
    procedure SetPTPJointParameters(const Velocity, Acceleration: Single; const Queued: Boolean = True);
    procedure SetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean = True);
    procedure SetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean = True);
    procedure SetPTPJumpParameters(const jumpHeight, zLimit: Single; const Queued: Boolean = True);
    procedure SetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single; const Queued: Boolean = True);
    procedure SetCommandTimeout(const Value: Integer);
    procedure SetQueuedCmdClear;
    procedure SetQueuedCmdStartExec;

    // --- States
    procedure GetDeviceVersion(out Major, Minor, Rev: Byte);
    procedure GetPose(out Pose: TPose);
    procedure GetAlarmState(out AlarmState: TArray<Byte>);
    function GetAlarms: TDobotAlarms;
    function GetCurrentCommandIndex: UInt64;

    // --- Actions
    procedure Home(const Queued: Boolean = True);
    procedure Stop(const Force: Boolean);

    // --- Move
    procedure Move(const Command: TDobotJogCommand; const MoveType: TDobotMoveType; const MoveForMS: Cardinal = 100; const Queued: Boolean = True);
    function Moving: Boolean;
    function LastMoveTicks: Cardinal;
    function GetAxisMovements: TDobotAxisMovements; 

    // --- Point to Point
    procedure SetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single; const Queued: Boolean = True);

    // --- EndExtenders
    procedure SetGripperPosition(const GripperPosition: TGripperPosition; const Queued: Boolean = True);
  published
    property OnLog;
    property MoveTickInterval;
  end;

implementation

resourcestring
  StrErrorCode = 'Error code: ';
  StrUnableToExecuteCo = 'Unable to execute command, Dobot is busy';
  StrDobotSerialPortNo = 'Dobot serial port not found';
  StrErrorConnectingTo = 'Error connecting to the Dobot serial port';
  StrConnecting = 'Connecting';
  StrConnected = 'Connected';
  StrDisconnecting = 'Disconnecting';
  StrDisconnected = 'Disconnected';
  StrDeviceVersionD = 'Device Version = %d.%d.%d';
  StrGetAlarms = 'Get Alarms';
  StrGetDevicePose = 'Get Device Pose';
  StrHome = 'Home';
  StrSMoveTypeSFor = '%s move type %s for %dms';
  StrCommandTimeoutSet = 'Command timeout set';
  StrCoordinateJogParam = 'Coordinate jog parameters set';
  StrGripperOff = 'Gripper Off';
  StrGripperOpen = 'Gripper Open';
  StrGripperClose = 'Gripper Close';
  StrJogCommonParameter = 'Jog common parameters set';
  StrJointJogParameters = 'Joint jog parameters set';
  StrPTPX58fY5 = 'PTP - X=%5.8f, Y=%5.8f, Z=%5.8f, R=%5.8f';
  StrPTPCommonParameter = 'PTP common parameters set';
  StrPTPCoordinateParam = 'PTP coordinate parameters set';
  StrPTPJointParameters = 'PTP joint parameters set';
  StrPTPJumpParameters = 'PTP jump parameters set';
  StrCommandQueueCleare = 'Command queue cleared';
  StrCommandStartExec = 'Command start exec';
  StrForceStop = 'Force Stop';
  StrStop = 'Stop';

function TBaseDobotController.Busy: Boolean;
begin
  DoBusy(Result);
end;

procedure TBaseDobotController.CheckBusy;
begin
  if Busy then
  begin
    raise EDobotCommandError.Create(StrUnableToExecuteCo);
  end;
end;

procedure TBaseDobotController.CheckResult(const Value: Integer);
begin
  NextCommandIndex;

  if Value <> 0 then
  begin
    DoLog(StrErrorCode + Value.ToString);

    raise EDobotCommandError.Create(StrErrorCode + Value.ToString);
  end;
end;

procedure TBaseDobotController.DoSetJointJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
var
  i: Integer;
  jointJOGParams: TJOGJointParams;
begin
  for i := 0 to 3 do
  begin
    jointJOGParams.velocity[i] := Velocity;
    jointJOGParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetJOGJointParams(@jointJOGParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetCoordinateJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
var
  i: Integer;
  coordinateJOGParams: TJOGcoordinateParams;
begin
  for i := 0 to 3 do
  begin
    coordinateJOGParams.velocity[i] := Velocity;
    coordinateJOGParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetJOGCoordinateParams(@coordinateJOGParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetGripperPosition(const GripperPosition: TGripperPosition; const Queued: Boolean);
begin
  case GripperPosition of
    Off:
      begin
        CheckResult(Dobot.DLL.SetEndEffectorGripper(False, False, Queued, @FMoveCmdIndex));
      end;
    Open:
      begin
        CheckResult(Dobot.DLL.SetEndEffectorGripper(True, False, Queued, @FMoveCmdIndex));
      end;
    Close:
      begin
        CheckResult(Dobot.DLL.SetEndEffectorGripper(True, True, Queued, @FMoveCmdIndex));
      end;
  end;
end;

procedure TBaseDobotController.DoSetPTPJointParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
var
  i: Integer;
  ptpJointParams: TptpJointParams;
begin
  for i := 0 to 3 do
  begin
    ptpJointParams.velocity[i] := Velocity;
    ptpJointParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetPTPJointParams(@ptpJointParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean);
var
  jogcommonParams: TJOGCommonParams;
begin
  jogcommonParams.velocityRatio := VelocityRatio;
  jogcommonParams.accelerationRatio := AccelerationRatio;

  CheckResult(Dobot.DLL.SetJOGCommonParams(@jogcommonParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single; const Queued: Boolean);
var
  PTPCmd: TPTPCmd;
begin
  PTPCmd.ptpMode := Byte(Mode);

  PTPCmd.x := X;
  PTPCmd.y := Y;
  PTPCmd.z := Z;
  PTPCmd.r := R;

  CheckResult(Dobot.DLL.SetPTPCmd(@PTPCmd, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean);
var
  ptpCommonParams: TptpCommonParams;
begin
  ptpCommonParams.velocityRatio := VelocityRatio;
  ptpCommonParams.accelerationRatio := AccelerationRatio;

  CheckResult(Dobot.DLL.SetPTPCommonParams(@ptpCommonParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single; const Queued: Boolean);
var
  ptpCoordinateParams: TptpCoordinateParams;
begin
  ptpCoordinateParams.xyzVelocity := xyzVelocity;
  ptpCoordinateParams.rVelocity := rVelocity;
  ptpCoordinateParams.xyzAcceleration := xyzAcceleration;
  ptpCoordinateParams.rAcceleration := rAcceleration;

  CheckResult(Dobot.DLL.SetPTPCoordinateParams(@ptpCoordinateParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetPTPJumpParameters(const jumpHeight, zLimit: Single; const Queued: Boolean);
var
  ptpJumpParams: TptpJumpParams;
begin
  ptpJumpParams.jumpHeight := jumpHeight;
  ptpJumpParams.zLimit := zLimit;

  CheckResult(Dobot.DLL.SetPTPJumpParams(@ptpJumpParams, Queued, @FMoveCmdIndex));
end;

procedure TBaseDobotController.DoSetCommandTimeout(const Value: Integer);
begin
  CheckResult(Dobot.DLL.SetCmdTimeout(Value));
end;

procedure TBaseDobotController.DoSetQueuedCmdClear;
begin
  CheckResult(Dobot.DLL.SetQueuedCmdClear);
end;

procedure TBaseDobotController.DoSetQueuedCmdStartExec;
begin
  CheckResult(Dobot.DLL.SetQueuedCmdStartExec);
end;

procedure TBaseDobotController.DoStop(const Force: Boolean);
begin
  if Force then
  begin
    CheckResult(Dobot.DLL.SetQueuedCmdForceStopExec);

    FMoveCmdIndex := DoGetCurrentCommandIndex;
    FNextTimerTicks := 0;
  end
  else
  begin
    CheckResult(Dobot.DLL.SetQueuedCmdStopExec);
  end;
end;

procedure TBaseDobotController.DoGetDeviceVersion(out Major, Minor, Rev: Byte);
var
  amajorV, aminorV, arev: Byte;
begin
  CheckResult(Dobot.DLL.GetDeviceVersion(@amajorV, @aminorV, @arev));

  Major := amajorv;
  Minor := aminorv;
  Rev := arev;
end;

procedure TBaseDobotController.DoGetPose(out Pose: TPose);
var
  AxisMovements: TDobotAxisMovements;
begin
  CheckResult(Dobot.DLL.getPose(@Pose));

  AxisMovements := [];
  
  if FLastPose.x <> Pose.x then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maX];
  end;
  
  if FLastPose.y <> Pose.y then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maY];
  end;
  
  if FLastPose.z <> Pose.z then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maZ];
  end;
  
  if FLastPose.r <> Pose.r then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maServo];
  end;
  
  if FLastPose.jointAngle[0] <> Pose.jointAngle[0] then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maJoint0];
  end;
  
  if FLastPose.jointAngle[1] <> Pose.jointAngle[1] then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maJoint1];
  end;
  
  if FLastPose.jointAngle[2] <> Pose.jointAngle[2] then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maJoint2];
  end;
  
  if FLastPose.jointAngle[3] <> Pose.jointAngle[3]  then
  begin
    AxisMovements := AxisMovements + [TDobotAxisMovement.maJoint3];
  end;
  
  if AxisMovements <> [] then
  begin
    FLastMoveTicks := TThread.GetTickCount;

    FAxisMovements := AxisMovements;
  end;

  FLastPose := Pose;
end;

procedure TBaseDobotController.DoHome(const Queued: Boolean = True);
var
  HomeCmd: THomeCmd;
begin
  CheckResult(Dobot.DLL.SetHOMECmd(@HomeCmd, Queued, @FMoveCmdIndex));
end;

function TBaseDobotController.DoLastMoveTicks: Cardinal;
begin
  Result := FLastMoveTicks;
end;

procedure TBaseDobotController.Loop;
begin
  DoLoop;
end;

procedure TBaseDobotController.DoLoop;
var
  DummyPose: TPose;
begin
  CheckTimer;

  DoGetPose(DummyPose);

  if not DoMoving then
  begin
    FAxisMovements := [];
  end;
end;

procedure TBaseDobotController.CheckTimer;
begin
  if (FNextTimerTicks <> 0) and
     (TThread.GetTickCount >= FNextTimerTicks) then
  begin
    FNextTimerTicks := 0;

    DoOnTimer;
  end;
end;

procedure TBaseDobotController.DoMove(const Command: TDobotJogCommand; const MoveType: TDobotMoveType;
  const MoveForMS: Cardinal; const Queued: Boolean);
var
  JOGCmd: TJOGCmd;
begin
  JOGCmd.cmd := Byte(Command);
  JOGCmd.isJoint := Byte(MoveType);

  CheckResult(Dobot.DLL.SetJOGCmd(@JOGCmd, Queued, @FMoveCmdIndex));

  FNextTimerTicks := 0;

  if MoveForMS <> 0 then
  begin
    FNextTimerTicks := TThread.GetTickCount + MoveForMS;
  end;
end;

function TBaseDobotController.DoMoving: Boolean;
begin
  Result := (FMoveCmdIndex <> DoGetCurrentCommandIndex) or (FNextTimerTicks <> 0);
end;

function TBaseDobotController.DoGetAlarms: TDobotAlarms;
var
  Bytes: TArray<Byte>;
begin
  DoGetAlarmState(Bytes);

  Result := DobotAlarmBytesToDobotAlarms(Bytes);
end;

procedure TBaseDobotController.DoGetAlarmState(out AlarmState: TArray<Byte>);
var
  Len: Integer;
begin
  Len := 32;

  SetLength(AlarmState, Len);

  CheckResult(Dobot.DLL.getAlarmsState(@AlarmState[0], @len, Len));
  CheckResult(Dobot.DLL.ClearAllAlarmsState);
end;

function TBaseDobotController.DoGetAxisMovements: TDobotAxisMovements;
begin
  Result := FAxisMovements;
end;

function TBaseDobotController.DoGetCurrentCommandIndex: UInt64;
begin
  Dobot.DLL.GetQueuedCmdCurrentIndex(@Result);
end;

procedure TBaseDobotController.DoConnect(const SerialPort: String; const BaudRate: Integer; const Timeout: Integer);
var
  ConnectResult: Integer;
begin
  ConnectResult := Dobot.DLL.ConnectDobot(
    PChar(SerialPort),
    Baudrate
  );

  DoSetCommandTimeout(Timeout);

  case ConnectResult of
    1: raise EDobotConnectError.Create(StrDobotSerialPortNo);
    2: raise EDobotConnectError.Create(StrErrorConnectingTo);
  end;

  DoSetJointJOGParameters(200, 200);
  DoSetCoordinateJOGParameters(200, 200);
  DoSetPTPJointParameters(200, 200);
  DoSetJogCommonParameters(50, 50);
  DoSetPTPCommonParameters(50, 50);
  DoSetPTPCoordinateParameters(200, 200, 200, 200);
  DoSetPTPJumpParameters(10, 20);

  DoSetQueuedCmdClear;
  DoSetQueuedCmdStartExec;

  DoLoop;
  
  FLastMoveTicks := 0;
  
  FConnected := True;
end;

function TBaseDobotController.DoConnected: Boolean;
begin
  Result := FConnected;
end;

constructor TBaseDobotController.Create(AOwner: TComponent);
begin
  inherited;

  FMoveTickInterval := 100;
end;

destructor TBaseDobotController.Destroy;
begin
  inherited;
end;

procedure TBaseDobotController.DoDisconnect;
begin
  Dobot.DLL.DisconnectDobot;

  FConnected := False;
end;

procedure TBaseDobotController.DoLog(const LogText: String; const LogLevel: TDobotLogLevel);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(Self, LogText, LogLevel);
  end;
end;

procedure TBaseDobotController.DoOnTimer;
begin
  DoMove(TDobotJogCommand.Release, TDobotMoveType.Linear, 0);
end;

procedure TBaseDobotController.DoBusy(out Value: Boolean);
begin
  Value := False;
end;

procedure TBaseDobotController.DoExec;
begin
  CheckResult(Dobot.DLL.DobotExec);
end;

procedure TBaseDobotController.NextCommandIndex;
begin
  Inc(FCmdIndex);
end;

{ TDobotController }

procedure TDobotController.Connect(const SerialPort: String; const BaudRate, Timeout: Integer);
begin
  CheckBusy;

  DoLog(StrConnecting, TDobotLogLevel.Debug1);

  DoConnect(SerialPort, BaudRate, Timeout);

  DoLog(StrConnected, TDobotLogLevel.Debug1);
end;

function TDobotController.Connected: Boolean;
begin
  Result := DoConnected;
end;

procedure TDobotController.Disconnect;
begin
  CheckBusy;

  DoLog(StrDisconnecting, TDobotLogLevel.Debug1);

  DoDisconnect;

  DoLog(StrDisconnected, TDobotLogLevel.Debug1);
end;

procedure TDobotController.Exec;
begin
  CheckBusy;

  DoExec;
end;

function TDobotController.GetAlarms: TDobotAlarms;
begin
  DoLog(StrGetAlarms, TDobotLogLevel.Debug3);

  Result := DoGetAlarms;
end;

procedure TDobotController.GetAlarmState(out AlarmState: TArray<Byte>);
begin
  DoGetAlarmState(AlarmState);
end;

function TDobotController.GetAxisMovements: TDobotAxisMovements;
begin
  Result := DoGetAxisMovements;
end;

function TDobotController.GetCurrentCommandIndex: UInt64;
begin
  Result := DoGetCurrentCommandIndex;
end;

procedure TDobotController.GetDeviceVersion(out Major, Minor, Rev: Byte);
begin
  DoGetDeviceVersion(Major, Minor, Rev);

  DoLog(format(StrDeviceVersionD, [Major, Minor, Rev]));
end;

procedure TDobotController.GetPose(out Pose: TPose);
begin
  DoLog(StrGetDevicePose, TDobotLogLevel.Debug3);

  DoGetPose(Pose);
end;

procedure TDobotController.Home(const Queued: Boolean);
begin
  CheckBusy;

  DoLog(StrHome, TDobotLogLevel.Debug1);

  DoHome(Queued);
end;

function TDobotController.LastMoveTicks: Cardinal;
begin
  Result := DoLastMoveTicks;
end;

procedure TDobotController.Move(const Command: TDobotJogCommand; const MoveType: TDobotMoveType; const MoveForMS: Cardinal; const Queued: Boolean);
begin
  CheckBusy;

  DoLog(format(StrSMoveTypeSFor, [DobotMoveTypeDescriptions[MoveType], DobotJogCommandDescriptions[Command], MoveForMS]), TDobotLogLevel.Debug1);

  DoMove(Command, MoveType, MoveForMS, Queued);
end;

function TDobotController.Moving: Boolean;
begin
  Result := DoMoving;
end;

procedure TDobotController.SetCommandTimeout(const Value: Integer);
begin
  CheckBusy;

  DoSetCommandTimeout(Value);

  DoLog(StrCommandTimeoutSet, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetCoordinateJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetCoordinateJOGParameters(Velocity, Acceleration, Queued);

  DoLog(StrCoordinateJogParam, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetGripperPosition(const GripperPosition: TGripperPosition; const Queued: Boolean);
begin
  CheckBusy;

  DoSetGripperPosition(GripperPosition, Queued);

  case GripperPosition of
    Off: DoLog(StrGripperOff, TDobotLogLevel.Debug1);
    Open: DoLog(StrGripperOpen, TDobotLogLevel.Debug1);
    Close: DoLog(StrGripperClose, TDobotLogLevel.Debug1);
  end;
end;

procedure TDobotController.SetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetJogCommonParameters(VelocityRatio, AccelerationRatio, Queued);

  DoLog(StrJogCommonParameter, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetJointJOGParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetJointJOGParameters(Velocity, Acceleration, Queued);

  DoLog(StrJointJogParameters, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoLog(format(StrPTPX58fY5, [X, Y, Z, R]), TDobotLogLevel.Debug1);

  DoSetPTP(Mode, X, Y, Z, R, Queued);
end;

procedure TDobotController.SetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetPTPCommonParameters(VelocityRatio, AccelerationRatio, Queued);

  DoLog(StrPTPCommonParameter, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetPTPCoordinateParameters(xyzVelocity, rVelocity, xyzAcceleration, rAcceleration, Queued);

  DoLog(StrPTPCoordinateParam, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTPJointParameters(const Velocity, Acceleration: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetPTPJointParameters(Velocity, Acceleration, Queued);

  DoLog(StrPTPJointParameters, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTPJumpParameters(const jumpHeight, zLimit: Single; const Queued: Boolean);
begin
  CheckBusy;

  DoSetPTPJumpParameters(jumpHeight, zLimit, Queued);

  DoLog(StrPTPJumpParameters, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetQueuedCmdClear;
begin
  CheckBusy;

  DoSetQueuedCmdClear;

  DoLog(StrCommandQueueCleare, TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetQueuedCmdStartExec;
begin
  CheckBusy;

  DoSetQueuedCmdStartExec;

  DoLog(StrCommandStartExec, TDobotLogLevel.Debug1);
end;

procedure TDobotController.Stop(const Force: Boolean);
begin
  CheckBusy;

  if Force then
  begin
    DoLog(StrForceStop, TDobotLogLevel.Debug1);
  end
  else
  begin
    DoLog(StrStop, TDobotLogLevel.Debug1);
  end;

  DoStop(Force);
end;

end.
